################################################################################
# Raw cold hardiness data to workable CSVs
# Elizabeth Houghton
# November 2026
################################################################################

##############################
# load packages into session
##############################
library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(purrr)

###############################
# 1 data processing function
##############################

process_LTE_file_1 <- function(file, winter_start_year) {
  
  # Read Excel file (headers are on row 2)
  data <- read_excel(file, skip = 1)
  
  # Reshape data from wide to long format
  data_long <- data %>%
    
    # Keep location, variety, and LTE measurement columns
    select(
      Location,
      Variety,
      starts_with("Avg LTE/site")
    ) %>%
    
    # Remove summary rows and other rows without a variety
    filter(
      !is.na(Location),
      !is.na(Variety)
    )   %>%
    
    # Convert LTE columns into rows
    pivot_longer(
      cols = starts_with("Avg LTE/site"),
      names_to = "date",
      values_to = "LTE",
      values_transform = list(LTE = as.character)
    ) %>% 
    
    # Extract month and day from column names
    mutate(
      date = str_extract(date, "(?<=\\().*(?=\\))"),
      month = str_extract(date, "^[A-Za-z]+"),
      day = as.integer(str_extract(date, "\\d+")),
      
      # Assign the correct calendar year
      year = if_else(
        month %in% c("Jan", "Feb", "Mar", "Apr",
                     "May", "Jun", "Jul", "Aug"),
        winter_start_year + 1L,
        winter_start_year
      ),
      
      # Create actual date
      date = make_date(
        year = year,
        month = match(
          month,
          month.abb
        ),
        day = day
      ),
      
      # Convert LTE values to numeric and supress warnings (for when NAs and such cause issues)
      LTE = suppressWarnings(as.numeric(LTE))
    ) %>%
    
    # Retain the four required columns
    select(
      location = Location,
      variety = Variety,
      date,
      LTE
    )
  
  return(data_long)
}

###############################
# 1 process data
##############################

LTE_2012_2013 <- process_LTE_file_1(
  file = "./01_data/cold_hardiness/2012_2013.xlsx",
  winter_start_year = 2012
)

LTE_2013_2014 <- process_LTE_file_1(
  file = "./01_data/cold_hardiness/2013_2014.xlsx",
  winter_start_year = 2013
)




###############################
# 2 data processing function
##############################
parse_LTE_date <- function(x, season_start_year) {
  
  x <- as.character(x)
  
  day <- rep(NA_integer_, length(x))
  month <- rep(NA_integer_, length(x))
  
  # Identify Excel numeric date serials
  serial <- !is.na(x) & grepl("^\\d+(\\.\\d+)?$", x)
  
  if (any(serial)) {
    d <- as.Date(
      as.numeric(x[serial]),
      origin = "1899-12-30"
    )
    
    day[serial] <- lubridate::day(d)
    month[serial] <- lubridate::month(d)
  }
  
  # Identify dates already formatted as YYYY-MM-DD
  iso <- !is.na(x) & grepl("^\\d{4}-\\d{2}-\\d{2}", x)
  
  if (any(iso)) {
    d <- as.Date(substr(x[iso], 1, 10))
    
    day[iso] <- lubridate::day(d)
    month[iso] <- lubridate::month(d)
  }
  
  # Identify dates formatted as DD-MMM (e.g. 12-Nov)
  dmy <- !is.na(x) & grepl("^\\d{1,2}-[A-Za-z]{3}", x)
  
  if (any(dmy)) {
    day[dmy] <- as.integer(
      stringr::str_extract(x[dmy], "^\\d{1,2}")
    )
    
    month[dmy] <- match(
      stringr::str_to_title(
        stringr::str_extract(x[dmy], "[A-Za-z]{3}")
      ),
      month.abb
    )
  }
  
  # Assign season years
  year <- ifelse(
    month >= 9,
    season_start_year,
    season_start_year + 1L
  )
  
  lubridate::make_date(
    year = year,
    month = month,
    day = day
  )
}

process_LTE_file_2 <- function(file) {
  
  # Extract starting year from filename
  season_start_year <- as.integer(
    stringr::str_extract(basename(file), "^\\d{4}")
  )
  
  if (is.na(season_start_year)) {
    stop("Could not extract season year from filename: ", file)
  }
  
  # Read Excel file
  data <- readxl::read_excel(
    file,
    sheet = 1,
    skip = 2,
    col_names = FALSE
  )
  
  # Determine number of LTE/date pairs
  n_pairs <- (ncol(data) - 2) / 2
  
  if (n_pairs != floor(n_pairs)) {
    stop("Unexpected number of columns in: ", file)
  }
  
  # Assign column names
  names(data) <- c(
    "location",
    "variety",
    as.vector(rbind(
      paste0("LTE_", seq_len(n_pairs)),
      paste0("date_", seq_len(n_pairs))
    ))
  )
  
  # Convert repeated columns to character
  # to prevent type conflicts
  data <- data %>%
    mutate(
      across(starts_with("LTE_"), as.character),
      across(starts_with("date_"), as.character)
    )
  
  # Reshape data
  data_long <- data %>%
    
    filter(
      !is.na(location),
      !is.na(variety)
    ) %>%
    
    pivot_longer(
      cols = -c(location, variety),
      names_to = c(".value", "measurement"),
      names_pattern = "(LTE|date)_(\\d+)"
    ) %>%
    
    mutate(
      # Assign correct season date
      date = parse_LTE_date(
        date,
        season_start_year
      ),
      
      # Convert LTE to numeric
      LTE = suppressWarnings(as.numeric(LTE))
    ) %>%
    
    # Remove missing dates
    filter(!is.na(date)) %>%
    
    select(
      location,
      variety,
      date,
      LTE
    )
  
  return(data_long)
}



##############################
# 2 process data
##############################

LTE_2014_2015 <- process_LTE_file_2(
  file = "./01_data/cold_hardiness/2014_2015.xlsx"
)

LTE_2015_2016 <- process_LTE_file_2(
  file = "./01_data/cold_hardiness/2015_2016.xlsx"
)

LTE_2016_2017 <- process_LTE_file_2(
  file = "./01_data/cold_hardiness/2016_2017.xlsx"
)


###############################
# 3 data processing function
##############################


process_LTE_file_3 <- function(file) {
  
  # Extract season start year from filename
  # Example: 2017_2018.xlsx -> 2017
  season_start_year <- as.integer(
    str_extract(
      basename(file),
      "^\\d{4}(?=_|\\.)"
    )
  )
  
  if (is.na(season_start_year)) {
    stop("Could not extract season year from filename: ", file)
  }
  
  # Read Excel file (headers on row 2)
  data <- read_excel(
    file,
    sheet = 1,
    skip = 1
  )
  
  # Identify all Avg LTE/vine columns
  LTE_columns <- names(data)[
    str_detect(names(data), "Avg") &
      str_detect(names(data), "LTE/vine")
  ]
  
  if (length(LTE_columns) == 0) {
    stop("No Avg LTE/vine columns found in: ", file)
  }
  
  # Reshape data into long format
  data_long <- data %>%
    
    # Remove summary rows
    filter(
      !is.na(Variety),
      !is.na(Location)
    ) %>%
    
    # Convert repeated LTE columns into rows
    pivot_longer(
      cols = all_of(LTE_columns),
      names_to = "date_header",
      values_to = "LTE",
      values_transform = list(LTE = as.character)
    ) %>%
    
    # Extract both dates from column headings
    mutate(
      date_text = str_match(
        date_header,
        "\\(([^)]+)\\)"
      )[, 2],
      
      date_parts = str_match(
        date_text,
        "^([A-Za-z]{3})\\s+(\\d{1,2})\\s*,\\s*(\\d{1,2})$"
      ),
      
      month = match(
        str_to_title(date_parts[, 2]),
        month.abb
      ),
      
      day1 = as.integer(date_parts[, 3]),
      day2 = as.integer(date_parts[, 4]),
      
      # Assign season years
      year = if_else(
        month >= 9,
        season_start_year,
        season_start_year + 1L
      ),
      
      # Create both possible sampling dates
      date1 = make_date(
        year = year,
        month = month,
        day = day1
      ),
      
      date2 = make_date(
        year = year,
        month = month,
        day = day2
      ),
      
      # Convert sample day to weekday number
      # Monday = 1, Tuesday = 2, etc.
      sample_weekday = match(
        str_sub(
          str_to_lower(str_trim(`sample day`)),
          1, 3
        ),
        c("mon", "tue", "wed", "thu",
          "fri", "sat", "sun")
      ),
      
      # Select the date matching the sample weekday
      date = case_when(
        wday(date1, week_start = 1) == sample_weekday ~ date1,
        wday(date2, week_start = 1) == sample_weekday ~ date2,
        TRUE ~ as.Date(NA)
      ),
      
      # Convert LTE to numeric
      LTE = suppressWarnings(as.numeric(LTE))
    )
  
  # Check for dates that could not be assigned
  if (any(!is.na(data_long$`sample day`) &
          is.na(data_long$date))) {
    warning(
      "Some sampling dates could not be matched to sample day in: ",
      file
    )
  }
  
  # Keep required columns
  data_long <- data_long %>%
    filter(!is.na(date)) %>%
    select(
      location = Location,
      variety = Variety,
      date,
      LTE
    )
  
  return(data_long)
}


##############################
# 3 process data
##############################

LTE_2017_2018 <- process_LTE_file_3(
  "./01_data/cold_hardiness/2017_2018.xlsx"
)

LTE_2018_2019 <- process_LTE_file_3(
  "./01_data/cold_hardiness/2018_2019.xlsx"
)

###############################
# 4 data processing function
##############################


process_LTE_file_4 <- function(file) {
  
  # Extract season start year from filename
  # Example: 2017_2018.xlsx -> 2017
  season_start_year <- as.integer(
    str_extract(
      basename(file),
      "^\\d{4}(?=_|\\.)"
    )
  )
  
  if (is.na(season_start_year)) {
    stop("Could not extract season year from filename: ", file)
  }
  
  # Read Excel file (headers on row 2)
  data <- read_excel(
    file,
    sheet = 1,
    skip = 1
  )
  
  # Identify all Avg LTE50 columns
  LTE_columns <- names(data)[
    str_detect(names(data), "Avg") &
      str_detect(names(data), "LTE50")
  ]
  
  if (length(LTE_columns) == 0) {
    stop("No Avg LT50 columns found in: ", file)
  }
  
  # Reshape data into long format
  data_long <- data %>%
    
    # Remove summary rows
    filter(
      !is.na(Variety),
      !is.na(Location)
    ) %>%
    
    # Convert repeated LTE columns into rows
    pivot_longer(
      cols = all_of(LTE_columns),
      names_to = "date_header",
      values_to = "LTE",
      values_transform = list(LTE = as.character)
    ) %>%
    
    # Extract both dates from column headings
    mutate(
      date_text = str_match(
        date_header,
        "\\(([^)]+)\\)"
      )[, 2],
      
      date_parts = str_match(
        date_text,
        "^([A-Za-z]{3})\\s+(\\d{1,2})\\s*,\\s*(\\d{1,2})$"
      ),
      
      month = match(
        str_to_title(date_parts[, 2]),
        month.abb
      ),
      
      day1 = as.integer(date_parts[, 3]),
      day2 = as.integer(date_parts[, 4]),
      
      # Assign season years
      year = if_else(
        month >= 9,
        season_start_year,
        season_start_year + 1L
      ),
      
      # Create both possible sampling dates
      date1 = make_date(
        year = year,
        month = month,
        day = day1
      ),
      
      date2 = make_date(
        year = year,
        month = month,
        day = day2
      ),
      
      # Convert sample day to weekday number
      # Monday = 1, Tuesday = 2, etc.
      sample_weekday = match(
        str_sub(
          str_to_lower(str_trim(`sample day`)),
          1, 3
        ),
        c("mon", "tue", "wed", "thu",
          "fri", "sat", "sun")
      ),
      
      # Select the date matching the sample weekday
      date = case_when(
        wday(date1, week_start = 1) == sample_weekday ~ date1,
        wday(date2, week_start = 1) == sample_weekday ~ date2,
        TRUE ~ as.Date(NA)
      ),
      
      # Convert LTE to numeric
      LTE = suppressWarnings(as.numeric(LTE))
    )
  
  # Check for dates that could not be assigned
  if (any(!is.na(data_long$`sample day`) &
          is.na(data_long$date))) {
    warning(
      "Some sampling dates could not be matched to sample day in: ",
      file
    )
  }
  
  # Keep required columns
  data_long <- data_long %>%
    filter(!is.na(date)) %>%
    select(
      location = Location,
      variety = Variety,
      date,
      LTE
    )
  
  return(data_long)
}


##############################
# 4 process data
##############################

LTE_2019_2020 <- process_LTE_file_4(
  "./01_data/cold_hardiness/2019_2020.xlsx"
)

LTE_2020_2021 <- process_LTE_file_4(
  "./01_data/cold_hardiness/2020_2021.xlsx"
)

LTE_2021_2022 <- process_LTE_file_4(
  "./01_data/cold_hardiness/2021_2022.xlsx"
)

LTE_2022_2023 <- process_LTE_file_4(
  "./01_data/cold_hardiness/2022_2023.xlsx"
)

LTE_2023_2024_1 <- process_LTE_file_4(
  "./01_data/cold_hardiness/2023_2024_1.xlsx"
)

LTE_2023_2024_2 <- process_LTE_file_4(
  "./01_data/cold_hardiness/2023_2024_2.xlsx"
)

LTE_2024_2025_1 <- process_LTE_file_4(
  "./01_data/cold_hardiness/2024_2025_1.xlsx"
)

LTE_2024_2025_2 <- process_LTE_file_4(
  "./01_data/cold_hardiness/2024_2025_2.xlsx"
)

LTE_2025_2026_1 <- process_LTE_file_4(
  "./01_data/cold_hardiness/2025_2026_1.xlsx"
)

LTE_2025_2026_2 <- process_LTE_file_4(
  "./01_data/cold_hardiness/2025_2026_2.xlsx"
)


#################################
# Add season identifier
################################
LTE_2012_2013$season <- "2012_2013"
LTE_2013_2014$season <- "2013_2014"
LTE_2014_2015$season <- "2014_2015"
LTE_2015_2016$season <- "2015_2016"
LTE_2016_2017$season <- "2016_2017"
LTE_2017_2018$season <- "2017_2018"
LTE_2018_2019$season <- "2018_2019"
LTE_2019_2020$season <- "2019_2020"
LTE_2020_2021$season <- "2020_2021"
LTE_2021_2022$season <- "2021_2022"
LTE_2022_2023$season <- "2022_2023"
LTE_2023_2024_1$season <- "2023_2024"
LTE_2023_2024_2$season <- "2023_2024"
LTE_2024_2025_1$season <- "2024_2025"
LTE_2024_2025_2$season <- "2024_2025"
LTE_2025_2026_1$season <- "2025_2026"
LTE_2025_2026_2$season <- "2025_2026"

#################################
# Merge all files in environment
################################

all_objs <- mget(ls())
df_list <- Filter(is.data.frame, all_objs)
combined_df <- do.call(rbind, df_list)

# drop any rows with na LTE
df_clean <- combined_df |> 
  filter(!is.na(LTE))

#################################
# Clean location 
################################

# Check unique values and frequencies in a single column
location <- df_clean %>%
  count(location, sort = TRUE)

# align similarly named locations

df_clean <- df_clean %>%
  mutate(
    location = location %>%
      str_to_lower() %>%                          # Make all lowercase
      str_replace_all("^ok falls", "okanagan falls") %>% # Expand "ok falls"
      str_remove_all(",") %>%                     # Remove commas
      str_squish() %>%                            # Clean up extra spaces
      str_replace_all(" ", "_")                   # Replace spaces with underscores
  )

# Check unique values and frequencies in a single column
location <- df_clean %>%
  count(location, sort = TRUE)

#################################
# Clean variety
################################
# Check unique values and frequencies in a single column
variety <- df_clean %>%
  count(variety, sort = TRUE)

df_clean <- df_clean %>%
  mutate(
    variety = variety %>%
      str_to_lower() %>%                          # Make all lowercase
      str_replace_all("^cabernet sauv(?!ignon)", "cabernet sauvignon") %>% # Expand shortforms
      str_replace_all("^sauv blanc", "sauvignon blanc") %>% # Expand shortforms
      str_replace_all("^marachel foch", "marechel foch") %>% # fix spelling error
      str_remove_all(",") %>%                     # Remove commas
      str_squish() %>%                            # Clean up extra spaces
      str_replace_all(" ", "_")                   # Replace spaces with underscores
    ) %>% 
  mutate(variety = str_remove_all(variety, fixed("("))) %>% 
  mutate(variety = str_remove_all(variety, fixed(")")))
  

variety <- df_clean %>%
  count(variety, sort = TRUE)


#################################
# Save csv
################################
write.csv(df_clean, file = "./02_outputs/LTE50.csv", row.names = FALSE)
