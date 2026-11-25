################################################################################
# Calculating parameters from weather data
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
library(zoo)
library(chillR)

##############################
# pull in weather data
##############################

kelowna_airport <- read.csv("./02_outputs/weather/kelowna_airport.csv") %>% 
  mutate(lat = 49.95722222)
kelowna_college <- read.csv("./02_outputs/weather/kelowna_college.csv") %>% 
  mutate(lat = 49.862336)
kelowna_ubco <- read.csv("./02_outputs/weather/kelowna_ubco.csv") %>% 
  mutate(lat = 49.940833)
osoyoos <- read.csv("./02_outputs/weather/osoyoos.csv") %>% 
  mutate(lat = 49.02833333)
penticton_airport <- read.csv("./02_outputs/weather/penticton_airport.csv") %>% 
  mutate(lat = 49.46305556)
summerland <- read.csv("./02_outputs/weather/summerland.csv") %>% 
  mutate(lat = 49.56255556)

##############################
# calculate parameters
##############################

########
# kelowna_airport
########

# chill
kelowna_airport_chill <- kelowna_airport %>%
  arrange(dt) %>%
  mutate(
    datetime = ymd_hms(dt), # adjust parser if format differs (e.g., ymd_hm)
    Year = year(dt),
    Month = month(dt),
    Day = day(dt),
    JDay = yday(dt),
    Hour = hour(dt),
    Temp = temp_filled,
    Date = as.Date(date),
    # Sept 1 - Dec 31 belongs to the season ending in Year + 1
    Season = if_else(Month >= 9, paste0(Year, "-", Year + 1), paste0(Year - 1, "-", Year))
  ) %>%
  select(dt, Date, Year, Month, Day, JDay, Hour, Temp, Season)



kelowna_airport_chill <- kelowna_airport_chill %>%
  group_by(Season) %>%
  arrange(dt, .by_group = TRUE) %>%
  mutate(
    # Cumulative metrics across the Sept 1 - Aug 31 season
    cum_CP = Dynamic_Model(Temp, summ = TRUE),
    cum_CH = Chilling_Hours(Temp, summ = TRUE),
    cum_Utah = Utah_Model(Temp, summ = TRUE),
    cum_GDH = GDH(Temp, summ = FALSE)
  ) %>%
  # Collapse hourly data to daily resolution
  group_by(Season, Date, Year, Month, Day) %>%
  summarise(
    Tmean = mean(Temp, na.rm = TRUE),
    Tmin = min(Temp, na.rm = TRUE),
    Tmax = max(Temp, na.rm = TRUE),
    # End-of-day cumulative chill for the growing season
    Cum_Chill_Portions = max(cum_CP, na.rm = TRUE),
    Cum_Chilling_Hours = max(cum_CH, na.rm = TRUE),
    Cum_Utah_Units = max(cum_Utah, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  # Calculate daily chill increments from the cumulative totals
  group_by(Season) %>%
  arrange(Date, .by_group = TRUE) %>%
  mutate(
    Daily_Chill_Portions = Cum_Chill_Portions - lag(Cum_Chill_Portions, default = 0),
    Daily_Chilling_Hours = Cum_Chilling_Hours - lag(Cum_Chilling_Hours, default = 0),
    Daily_Utah_Units = Cum_Utah_Units - lag(Cum_Utah_Units, default = 0),
  ) %>%
  ungroup() %>%
  select(Date, Season, Cum_Chill_Portions, Month, Day) %>% 
  rename(date = Date,
         month = Month,
         day = Day,
         season = Season,
         cum_chill_portions = Cum_Chill_Portions)

# temperatures
kelowna_airport <- kelowna_airport %>% 
  mutate(date = as.Date(date),
         year = year(date)) %>%
  
  # 2. Aggregate to daily mean, min, and max
  group_by(year, date) %>%
  summarise(
    temp_mean = mean(temp_filled, na.rm = TRUE),
    temp_min  = min(temp_filled, na.rm = TRUE),
    temp_max  = max(temp_filled, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  
  mutate(
    temp_avg = ((temp_min+temp_max)/2),
    jday = yday(date)
  ) %>% 
  
  # 3. Sort chronologically so lags align with the previous calendar days
  arrange(date) %>%
  
  # 4. Create lag columns for 1 to 7 days prior across mean, min, and max
  mutate(
    across(
      .cols = c(temp_mean, temp_min, temp_max),
      .fns = setNames(
        lapply(1:7, function(k) ~ lag(.x, n = k)),
        paste0("lag", 1:7)
      ),
      .names = "{.col}_{.fn}"
    )) %>% 
  mutate(
    daylength_hrs = daylength(latitude = 49.95722222, JDay = jday)$Daylength
  )

# merge
kelowna_airport <- merge(kelowna_airport_chill, kelowna_airport, by = "date", all.x = TRUE)



########
# kelowna_college
########

# chill
kelowna_college_chill <- kelowna_college %>%
  arrange(dt) %>%
  mutate(
    datetime = ymd_hms(dt), # adjust parser if format differs (e.g., ymd_hm)
    Year = year(dt),
    Month = month(dt),
    Day = day(dt),
    JDay = yday(dt),
    Hour = hour(dt),
    Temp = temp_filled,
    Date = as.Date(date),
    # Sept 1 - Dec 31 belongs to the season ending in Year + 1
    Season = if_else(Month >= 9, paste0(Year, "-", Year + 1), paste0(Year - 1, "-", Year))
  ) %>%
  select(dt, Date, Year, Month, Day, JDay, Hour, Temp, Season)



kelowna_college_chill <- kelowna_college_chill %>%
  group_by(Season) %>%
  arrange(dt, .by_group = TRUE) %>%
  mutate(
    # Cumulative metrics across the Sept 1 - Aug 31 season
    cum_CP = Dynamic_Model(Temp, summ = TRUE),
    cum_CH = Chilling_Hours(Temp, summ = TRUE),
    cum_Utah = Utah_Model(Temp, summ = TRUE),
    cum_GDH = GDH(Temp, summ = FALSE)
  ) %>%
  # Collapse hourly data to daily resolution
  group_by(Season, Date, Year, Month, Day) %>%
  summarise(
    Tmean = mean(Temp, na.rm = TRUE),
    Tmin = min(Temp, na.rm = TRUE),
    Tmax = max(Temp, na.rm = TRUE),
    # End-of-day cumulative chill for the growing season
    Cum_Chill_Portions = max(cum_CP, na.rm = TRUE),
    Cum_Chilling_Hours = max(cum_CH, na.rm = TRUE),
    Cum_Utah_Units = max(cum_Utah, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  # Calculate daily chill increments from the cumulative totals
  group_by(Season) %>%
  arrange(Date, .by_group = TRUE) %>%
  mutate(
    Daily_Chill_Portions = Cum_Chill_Portions - lag(Cum_Chill_Portions, default = 0),
    Daily_Chilling_Hours = Cum_Chilling_Hours - lag(Cum_Chilling_Hours, default = 0),
    Daily_Utah_Units = Cum_Utah_Units - lag(Cum_Utah_Units, default = 0),
  ) %>%
  ungroup() %>%
  select(Date, Season, Cum_Chill_Portions, Month, Day) %>% 
  rename(date = Date,
         month = Month,
         day = Day,
         season = Season,
         cum_chill_portions = Cum_Chill_Portions)

# temperatures
kelowna_college <- kelowna_college %>% 
  mutate(date = as.Date(date),
         year = year(date)) %>%
  
  # 2. Aggregate to daily mean, min, and max
  group_by(year, date) %>%
  summarise(
    temp_mean = mean(temp_filled, na.rm = TRUE),
    temp_min  = min(temp_filled, na.rm = TRUE),
    temp_max  = max(temp_filled, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  
  mutate(
    temp_avg = ((temp_min+temp_max)/2),
    jday = yday(date)
  ) %>% 
  
  # 3. Sort chronologically so lags align with the previous calendar days
  arrange(date) %>%
  
  # 4. Create lag columns for 1 to 7 days prior across mean, min, and max
  mutate(
    across(
      .cols = c(temp_mean, temp_min, temp_max),
      .fns = setNames(
        lapply(1:7, function(k) ~ lag(.x, n = k)),
        paste0("lag", 1:7)
      ),
      .names = "{.col}_{.fn}"
    )) %>% 
  mutate(
    daylength_hrs = daylength(latitude = 49.862336, JDay = jday)$Daylength
  )

# merge
kelowna_college <- merge(kelowna_college_chill, kelowna_college, by = "date", all.x = TRUE)


########
# kelowna_ubco
########

# chill
kelowna_ubco_chill <- kelowna_ubco %>%
  arrange(dt) %>%
  mutate(
    datetime = ymd_hms(dt), # adjust parser if format differs (e.g., ymd_hm)
    Year = year(dt),
    Month = month(dt),
    Day = day(dt),
    JDay = yday(dt),
    Hour = hour(dt),
    Temp = temp_filled,
    Date = as.Date(date),
    # Sept 1 - Dec 31 belongs to the season ending in Year + 1
    Season = if_else(Month >= 9, paste0(Year, "-", Year + 1), paste0(Year - 1, "-", Year))
  ) %>%
  select(dt, Date, Year, Month, Day, JDay, Hour, Temp, Season)



kelowna_ubco_chill <- kelowna_ubco_chill %>%
  group_by(Season) %>%
  arrange(dt, .by_group = TRUE) %>%
  mutate(
    # Cumulative metrics across the Sept 1 - Aug 31 season
    cum_CP = Dynamic_Model(Temp, summ = TRUE),
    cum_CH = Chilling_Hours(Temp, summ = TRUE),
    cum_Utah = Utah_Model(Temp, summ = TRUE),
    cum_GDH = GDH(Temp, summ = FALSE)
  ) %>%
  # Collapse hourly data to daily resolution
  group_by(Season, Date, Year, Month, Day) %>%
  summarise(
    Tmean = mean(Temp, na.rm = TRUE),
    Tmin = min(Temp, na.rm = TRUE),
    Tmax = max(Temp, na.rm = TRUE),
    # End-of-day cumulative chill for the growing season
    Cum_Chill_Portions = max(cum_CP, na.rm = TRUE),
    Cum_Chilling_Hours = max(cum_CH, na.rm = TRUE),
    Cum_Utah_Units = max(cum_Utah, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  # Calculate daily chill increments from the cumulative totals
  group_by(Season) %>%
  arrange(Date, .by_group = TRUE) %>%
  mutate(
    Daily_Chill_Portions = Cum_Chill_Portions - lag(Cum_Chill_Portions, default = 0),
    Daily_Chilling_Hours = Cum_Chilling_Hours - lag(Cum_Chilling_Hours, default = 0),
    Daily_Utah_Units = Cum_Utah_Units - lag(Cum_Utah_Units, default = 0),
  ) %>%
  ungroup() %>%
  select(Date, Season, Cum_Chill_Portions, Month, Day) %>% 
  rename(date = Date,
         month = Month,
         day = Day,
         season = Season,
         cum_chill_portions = Cum_Chill_Portions)

# temperatures
kelowna_ubco <- kelowna_ubco %>% 
  mutate(date = as.Date(date),
         year = year(date)) %>%
  
  # 2. Aggregate to daily mean, min, and max
  group_by(year, date) %>%
  summarise(
    temp_mean = mean(temp_filled, na.rm = TRUE),
    temp_min  = min(temp_filled, na.rm = TRUE),
    temp_max  = max(temp_filled, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  
  mutate(
    temp_avg = ((temp_min+temp_max)/2),
    jday = yday(date)
  ) %>% 
  
  # 3. Sort chronologically so lags align with the previous calendar days
  arrange(date) %>%
  
  # 4. Create lag columns for 1 to 7 days prior across mean, min, and max
  mutate(
    across(
      .cols = c(temp_mean, temp_min, temp_max),
      .fns = setNames(
        lapply(1:7, function(k) ~ lag(.x, n = k)),
        paste0("lag", 1:7)
      ),
      .names = "{.col}_{.fn}"
    )) %>% 
  mutate(
    daylength_hrs = daylength(latitude = 49.940833, JDay = jday)$Daylength
  )

# merge
kelowna_ubco <- merge(kelowna_ubco_chill, kelowna_ubco, by = "date", all.x = TRUE)



########
# osoyoos
########

# chill
osoyoos_chill <- osoyoos %>%
  arrange(dt) %>%
  mutate(
    datetime = ymd_hms(dt), # adjust parser if format differs (e.g., ymd_hm)
    Year = year(dt),
    Month = month(dt),
    Day = day(dt),
    JDay = yday(dt),
    Hour = hour(dt),
    Temp = temp_filled,
    Date = as.Date(date),
    # Sept 1 - Dec 31 belongs to the season ending in Year + 1
    Season = if_else(Month >= 9, paste0(Year, "-", Year + 1), paste0(Year - 1, "-", Year))
  ) %>%
  select(dt, Date, Year, Month, Day, JDay, Hour, Temp, Season)



osoyoos_chill <- osoyoos_chill %>%
  group_by(Season) %>%
  arrange(dt, .by_group = TRUE) %>%
  mutate(
    # Cumulative metrics across the Sept 1 - Aug 31 season
    cum_CP = Dynamic_Model(Temp, summ = TRUE),
    cum_CH = Chilling_Hours(Temp, summ = TRUE),
    cum_Utah = Utah_Model(Temp, summ = TRUE),
    cum_GDH = GDH(Temp, summ = FALSE)
  ) %>%
  # Collapse hourly data to daily resolution
  group_by(Season, Date, Year, Month, Day) %>%
  summarise(
    Tmean = mean(Temp, na.rm = TRUE),
    Tmin = min(Temp, na.rm = TRUE),
    Tmax = max(Temp, na.rm = TRUE),
    # End-of-day cumulative chill for the growing season
    Cum_Chill_Portions = max(cum_CP, na.rm = TRUE),
    Cum_Chilling_Hours = max(cum_CH, na.rm = TRUE),
    Cum_Utah_Units = max(cum_Utah, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  # Calculate daily chill increments from the cumulative totals
  group_by(Season) %>%
  arrange(Date, .by_group = TRUE) %>%
  mutate(
    Daily_Chill_Portions = Cum_Chill_Portions - lag(Cum_Chill_Portions, default = 0),
    Daily_Chilling_Hours = Cum_Chilling_Hours - lag(Cum_Chilling_Hours, default = 0),
    Daily_Utah_Units = Cum_Utah_Units - lag(Cum_Utah_Units, default = 0),
  ) %>%
  ungroup() %>%
  select(Date, Season, Cum_Chill_Portions, Month, Day) %>% 
  rename(date = Date,
         month = Month,
         day = Day,
         season = Season,
         cum_chill_portions = Cum_Chill_Portions)

# temperatures
osoyoos <- osoyoos %>% 
  mutate(date = as.Date(date),
         year = year(date)) %>%
  
  # 2. Aggregate to daily mean, min, and max
  group_by(year, date) %>%
  summarise(
    temp_mean = mean(temp_filled, na.rm = TRUE),
    temp_min  = min(temp_filled, na.rm = TRUE),
    temp_max  = max(temp_filled, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  
  mutate(
    temp_avg = ((temp_min+temp_max)/2),
    jday = yday(date)
  ) %>% 
  
  # 3. Sort chronologically so lags align with the previous calendar days
  arrange(date) %>%
  
  # 4. Create lag columns for 1 to 7 days prior across mean, min, and max
  mutate(
    across(
      .cols = c(temp_mean, temp_min, temp_max),
      .fns = setNames(
        lapply(1:7, function(k) ~ lag(.x, n = k)),
        paste0("lag", 1:7)
      ),
      .names = "{.col}_{.fn}"
    )) %>% 
  mutate(
    daylength_hrs = daylength(latitude = 49.02833333, JDay = jday)$Daylength
  )

# merge
osoyoos <- merge(osoyoos_chill, osoyoos, by = "date", all.x = TRUE)



########
# penticton_airport
########

# chill
penticton_airport_chill <- penticton_airport %>%
  arrange(dt) %>%
  mutate(
    datetime = ymd_hms(dt), # adjust parser if format differs (e.g., ymd_hm)
    Year = year(dt),
    Month = month(dt),
    Day = day(dt),
    JDay = yday(dt),
    Hour = hour(dt),
    Temp = temp_filled,
    Date = as.Date(date),
    # Sept 1 - Dec 31 belongs to the season ending in Year + 1
    Season = if_else(Month >= 9, paste0(Year, "-", Year + 1), paste0(Year - 1, "-", Year))
  ) %>%
  select(dt, Date, Year, Month, Day, JDay, Hour, Temp, Season)



penticton_airport_chill <- penticton_airport_chill %>%
  group_by(Season) %>%
  arrange(dt, .by_group = TRUE) %>%
  mutate(
    # Cumulative metrics across the Sept 1 - Aug 31 season
    cum_CP = Dynamic_Model(Temp, summ = TRUE),
    cum_CH = Chilling_Hours(Temp, summ = TRUE),
    cum_Utah = Utah_Model(Temp, summ = TRUE),
    cum_GDH = GDH(Temp, summ = FALSE)
  ) %>%
  # Collapse hourly data to daily resolution
  group_by(Season, Date, Year, Month, Day) %>%
  summarise(
    Tmean = mean(Temp, na.rm = TRUE),
    Tmin = min(Temp, na.rm = TRUE),
    Tmax = max(Temp, na.rm = TRUE),
    # End-of-day cumulative chill for the growing season
    Cum_Chill_Portions = max(cum_CP, na.rm = TRUE),
    Cum_Chilling_Hours = max(cum_CH, na.rm = TRUE),
    Cum_Utah_Units = max(cum_Utah, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  # Calculate daily chill increments from the cumulative totals
  group_by(Season) %>%
  arrange(Date, .by_group = TRUE) %>%
  mutate(
    Daily_Chill_Portions = Cum_Chill_Portions - lag(Cum_Chill_Portions, default = 0),
    Daily_Chilling_Hours = Cum_Chilling_Hours - lag(Cum_Chilling_Hours, default = 0),
    Daily_Utah_Units = Cum_Utah_Units - lag(Cum_Utah_Units, default = 0),
  ) %>%
  ungroup() %>%
  select(Date, Season, Cum_Chill_Portions, Month, Day) %>% 
  rename(date = Date,
         month = Month,
         day = Day,
         season = Season,
         cum_chill_portions = Cum_Chill_Portions)

# temperatures
penticton_airport <- penticton_airport %>% 
  mutate(date = as.Date(date),
         year = year(date)) %>%
  
  # 2. Aggregate to daily mean, min, and max
  group_by(year, date) %>%
  summarise(
    temp_mean = mean(temp_filled, na.rm = TRUE),
    temp_min  = min(temp_filled, na.rm = TRUE),
    temp_max  = max(temp_filled, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  
  mutate(
    temp_avg = ((temp_min+temp_max)/2),
    jday = yday(date)
  ) %>% 
  
  # 3. Sort chronologically so lags align with the previous calendar days
  arrange(date) %>%
  
  # 4. Create lag columns for 1 to 7 days prior across mean, min, and max
  mutate(
    across(
      .cols = c(temp_mean, temp_min, temp_max),
      .fns = setNames(
        lapply(1:7, function(k) ~ lag(.x, n = k)),
        paste0("lag", 1:7)
      ),
      .names = "{.col}_{.fn}"
    )) %>% 
  mutate(
    daylength_hrs = daylength(latitude = 49.46305556, JDay = jday)$Daylength
  )

# merge
penticton_airport <- merge(penticton_airport_chill, penticton_airport, by = "date", all.x = TRUE)


########
# summerland
########

# chill
summerland_chill <- summerland %>%
  arrange(dt) %>%
  mutate(
    datetime = ymd_hms(dt), # adjust parser if format differs (e.g., ymd_hm)
    Year = year(dt),
    Month = month(dt),
    Day = day(dt),
    JDay = yday(dt),
    Hour = hour(dt),
    Temp = temp_filled,
    Date = as.Date(date),
    # Sept 1 - Dec 31 belongs to the season ending in Year + 1
    Season = if_else(Month >= 9, paste0(Year, "-", Year + 1), paste0(Year - 1, "-", Year))
  ) %>%
  select(dt, Date, Year, Month, Day, JDay, Hour, Temp, Season)



summerland_chill <- summerland_chill %>%
  group_by(Season) %>%
  arrange(dt, .by_group = TRUE) %>%
  mutate(
    # Cumulative metrics across the Sept 1 - Aug 31 season
    cum_CP = Dynamic_Model(Temp, summ = TRUE),
    cum_CH = Chilling_Hours(Temp, summ = TRUE),
    cum_Utah = Utah_Model(Temp, summ = TRUE),
    cum_GDH = GDH(Temp, summ = FALSE)
  ) %>%
  # Collapse hourly data to daily resolution
  group_by(Season, Date, Year, Month, Day) %>%
  summarise(
    Tmean = mean(Temp, na.rm = TRUE),
    Tmin = min(Temp, na.rm = TRUE),
    Tmax = max(Temp, na.rm = TRUE),
    # End-of-day cumulative chill for the growing season
    Cum_Chill_Portions = max(cum_CP, na.rm = TRUE),
    Cum_Chilling_Hours = max(cum_CH, na.rm = TRUE),
    Cum_Utah_Units = max(cum_Utah, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  # Calculate daily chill increments from the cumulative totals
  group_by(Season) %>%
  arrange(Date, .by_group = TRUE) %>%
  mutate(
    Daily_Chill_Portions = Cum_Chill_Portions - lag(Cum_Chill_Portions, default = 0),
    Daily_Chilling_Hours = Cum_Chilling_Hours - lag(Cum_Chilling_Hours, default = 0),
    Daily_Utah_Units = Cum_Utah_Units - lag(Cum_Utah_Units, default = 0),
  ) %>%
  ungroup() %>%
  select(Date, Season, Cum_Chill_Portions, Month, Day) %>% 
  rename(date = Date,
         month = Month,
         day = Day,
         season = Season,
         cum_chill_portions = Cum_Chill_Portions)

# temperatures
summerland <- summerland %>% 
  mutate(date = as.Date(date),
         year = year(date)) %>%
  
  # 2. Aggregate to daily mean, min, and max
  group_by(year, date) %>%
  summarise(
    temp_mean = mean(temp_filled, na.rm = TRUE),
    temp_min  = min(temp_filled, na.rm = TRUE),
    temp_max  = max(temp_filled, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  
  mutate(
    temp_avg = ((temp_min+temp_max)/2),
    jday = yday(date)
  ) %>% 
  
  # 3. Sort chronologically so lags align with the previous calendar days
  arrange(date) %>%
  
  # 4. Create lag columns for 1 to 7 days prior across mean, min, and max
  mutate(
    across(
      .cols = c(temp_mean, temp_min, temp_max),
      .fns = setNames(
        lapply(1:7, function(k) ~ lag(.x, n = k)),
        paste0("lag", 1:7)
      ),
      .names = "{.col}_{.fn}"
    )) %>% 
  mutate(
    daylength_hrs = daylength(latitude = 49.56255556, JDay = jday)$Daylength
  )

# merge
summerland <- merge(summerland_chill, summerland, by = "date", all.x = TRUE)

##############################
# export cleaned weather data
##############################

write.csv(kelowna_airport, file = "./02_outputs/daily_weather_parameters/kelowna_airport.csv", row.names = FALSE)
write.csv(kelowna_college, file = "./02_outputs/daily_weather_parameters/kelowna_college.csv", row.names = FALSE)
write.csv(kelowna_ubco, file = "./02_outputs/daily_weather_parameters/kelowna_ubco.csv", row.names = FALSE)
write.csv(osoyoos, file = "./02_outputs/daily_weather_parameters/osoyoos.csv", row.names = FALSE)
write.csv(penticton_airport, file = "./02_outputs/daily_weather_parameters/penticton_airport.csv", row.names = FALSE)
write.csv(summerland, file = "./02_outputs/daily_weather_parameters/summerland.csv", row.names = FALSE)

