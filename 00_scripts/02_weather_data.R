################################################################################
# Cleaning weather data
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

##############################
# pull in LTE data
##############################

LTE <- read.csv("./02_outputs/LTE50.csv")

##############################
# identify locations
##############################
location <- LTE %>%
  count(location, sort = TRUE)

# PCIC hourly weather stations in the Okanagan/Similkameen
# https://services.pacificclimate.org/met-data-portal-pcds/app?view=1.eyJuIjpbMTAsMTcsNSw3MCw3MSwzNiwxLDE5LDksMTQsMTEsMTIsMTYsMiwzLDE1LDcyLDc0LDc1LDc2LDYsMzVdLCJ2IjpbMTMwNyw0MjgsNDMzLDQ1Miw0NjQsNDcwLDQ5NCw1MTIsNTE4LDUzMCw1NDQsNzIwLDQ3Miw1MjksNTQwLDY5Myw3MDEsNzAyLDcyMSw3OTIsODAwLDEyOTUsMTI5NywxMzA1LDEzMDYsMTMyOCwxMzI5LDEzMzYsMTMwOCw0MjcsNDM1LDQ1NCw0NjMsNDY5LDQ5Myw1MTEsNTE3LDUzMSw1NDUsNzIyLDQzNCw0MzYsNDUzLDQ5Nyw1MDMsNTQyLDY1Nyw2NTgsMTMxNCwxMzM5LDEzNDUsMTI2OCw1NTYsNTYxLDU2NSw1NjksNTczLDU3Nyw1ODEsNTg1LDU4OSw1OTcsNjAxLDYwNSw2MDksNjEzLDU1OCw1NjMsNTY3LDU3MSw1NzUsNTc5LDU4Myw1ODcsNTkxLDU5OSw2MDMsNjA3LDYxMSw2MTUsNTU3LDU2Miw1NjYsNTcwLDU3NCw1NzgsNTgyLDU4Niw1OTAsNTk4LDYwMiw2MDYsNjEwLDYxNF0sImYiOlsiMS1ob3VybHkiXSwicyI6IjIwMTItMDktMDFUMDc6MDA6MDAuMDAwWiIsImUiOiIyMDI2LTEyLTAxVDA3OjAwOjAwLjAwMFoiLCJtIjpbNDkuMDMwNjE1LC0xMTkuNTAwOTYxLDEzXSwiaSI6NjYyM30&table=1&filters=1

#################
# kelowna airport
#################
# lake_country

#################
# kelowna ubco
#################
# north_kelowna

#################
# kelowna college
#################
# kelowna
# south_kelowna
# east_kelowna
# west_kelowna

#################
# osoyoos
#################
# osoyoos_north
# osoyoos_west
# osoyoos_northeast
# osoyoos_southeast
# oliver
# oliver_north
# oliver_west
# oliver_south
# oliver_east
# black_sage
# similkameen

#################
# penticton airport
#################
# okanagan_falls_east
# okanagan_falls_west
# kaleden
# naramata_bench

#################
# penticton rs
#################
# too many errors/gaps/dirty data

#################
# summerland
#################
# summerland
# peachland

##############################
# pull in weather data
##############################

kelowna_airport_1 <- read.csv("./01_data/weather/EC_raw/kelowna_airport_1.csv") %>% 
  slice(-(1:137)) %>% 
  select(obs_time, air_temperature)
kelowna_airport_2 <- read.csv("./01_data/weather/EC_raw/kelowna_airport_2.csv") %>% 
  select(obs_time, air_temperature)

kelowna_airport <- rbind(kelowna_airport_2, kelowna_airport_1)

kelowna_ubco <- read.csv("./01_data/weather/EC_raw/kelowna_ubco.csv") %>% 
  select(obs_time, air_temperature)
kelowna_college <- read.csv("./01_data/weather/ENV-AQN/kelowna_college.csv")%>% 
  select(obs_time, avg_air_temp_pst1hr) %>% 
  rename(air_temperature = avg_air_temp_pst1hr)
osoyoos <- read.csv("./01_data/weather/EC_raw/osoyoos.csv") %>% 
  select(obs_time, air_temperature)

penticton_airport_1 <- read.csv("./01_data/weather/EC_raw/penticton_airport_1.csv") 
penticton_airport_2 <- read.csv("./01_data/weather/EC_raw/penticton_airport_2.csv") %>% 
  select(-rain_amount) %>% 
  slice(-(1:7))
penticton_airport <- rbind(penticton_airport_1, penticton_airport_2) %>% 
  select(obs_time, air_temperature)

summerland <- read.csv("./01_data/weather/EC_raw/summerland.csv") %>% 
  select(obs_time, air_temperature)


##############################
# clean weather data
##############################

# 1. Define your exact start and end timestamps (always specify tz = "UTC" to avoid DST gaps)
start_dt <- ymd_hms("2012-09-01 00:00:00", tz = "UTC")
end_dt   <- ymd_hms("2026-11-23 10:00:00", tz = "UTC")


kelowna_airport <- kelowna_airport %>%
  mutate(
    # 1. Convert string to POSIXct datetime (defaults to UTC)
    dt = ymd_hms(obs_time),
    
    # 2. Extract Date (returns a Date object: 2026-11-23)
    date = as_date(dt),
    
    # 3. Extract Time as a formatted character string ("10:00:00")
    time = format(dt, "%H:%M:%S"),
    hour = format(dt, "%H")) %>% 
    
    # 4. set dt format to match with start and end
      mutate(dt = as.POSIXct(dt, tz = "UTC")) %>%
      
      # Fill in any missing hours (missing rows get NA for other columns)
      complete(dt = seq(from = start_dt, to = end_dt, by = "1 hour")) %>%
      
      # Calculate the Sept 1 – Aug 31 season
      mutate(
        start_year = if_else(month(dt) >= 9, year(dt), year(dt) - 1),
        season = paste0(start_year, "-", start_year + 1)
      ) %>%
      select(-start_year) %>% 
      
      # sort by timestamp
      arrange(dt) %>%
      mutate(temp_filled = na.approx(air_temperature, na.rm = FALSE),
             temp_filled = round(temp_filled, 1),
             
             # 3. Extract Time as a formatted character string ("10:00:00")
             time = format(dt, "%H:%M:%S"),
             hour = format(dt, "%H")) %>% 
    filter(!is.na(obs_time))
  

kelowna_college <- kelowna_college %>%
  mutate(
    # 1. Convert string to POSIXct datetime (defaults to UTC)
    dt = ymd_hms(obs_time),
    
    # 2. Extract Date (returns a Date object: 2026-11-23)
    date = as_date(dt),
    
    # 3. Extract Time as a formatted character string ("10:00:00")
    time = format(dt, "%H:%M:%S"),
    hour = format(dt, "%H")) %>% 
  
  # 4. set dt format to match with start and end
  mutate(dt = as.POSIXct(dt, tz = "UTC")) %>%
  
  # Fill in any missing hours (missing rows get NA for other columns)
  complete(dt = seq(from = start_dt, to = end_dt, by = "1 hour")) %>%
  
  # Calculate the Sept 1 – Aug 31 season
  mutate(
    start_year = if_else(month(dt) >= 9, year(dt), year(dt) - 1),
    season = paste0(start_year, "-", start_year + 1)
  ) %>%
  select(-start_year) %>% 
  
  # sort by timestamp
  arrange(dt) %>%
  mutate(temp_filled = na.approx(air_temperature, na.rm = FALSE),
         temp_filled = round(temp_filled, 1),
         
         # 3. Extract Time as a formatted character string ("10:00:00")
         time = format(dt, "%H:%M:%S"),
         hour = format(dt, "%H")) %>% 
  filter(!is.na(obs_time))


kelowna_ubco <- kelowna_ubco %>%
  mutate(
    # 1. Convert string to POSIXct datetime (defaults to UTC)
    dt = ymd_hms(obs_time),
    
    # 2. Extract Date (returns a Date object: 2026-11-23)
    date = as_date(dt),
    
    # 3. Extract Time as a formatted character string ("10:00:00")
    time = format(dt, "%H:%M:%S"),
    hour = format(dt, "%H")) %>% 
  
  # 4. set dt format to match with start and end
  mutate(dt = as.POSIXct(dt, tz = "UTC")) %>%
  
  # Fill in any missing hours (missing rows get NA for other columns)
  complete(dt = seq(from = start_dt, to = end_dt, by = "1 hour")) %>%
  
  # Calculate the Sept 1 – Aug 31 season
  mutate(
    start_year = if_else(month(dt) >= 9, year(dt), year(dt) - 1),
    season = paste0(start_year, "-", start_year + 1)
  ) %>%
  select(-start_year) %>% 
  
  # sort by timestamp
  arrange(dt) %>%
  mutate(temp_filled = na.approx(air_temperature, na.rm = FALSE),
         temp_filled = round(temp_filled, 1),
         
         # 3. Extract Time as a formatted character string ("10:00:00")
         time = format(dt, "%H:%M:%S"),
         hour = format(dt, "%H")) %>% 
  filter(!is.na(obs_time))

osoyoos <- osoyoos %>%
  mutate(
    # 1. Convert string to POSIXct datetime (defaults to UTC)
    dt = ymd_hms(obs_time),
    
    # 2. Extract Date (returns a Date object: 2026-11-23)
    date = as_date(dt),
    
    # 3. Extract Time as a formatted character string ("10:00:00")
    time = format(dt, "%H:%M:%S"),
    hour = format(dt, "%H")) %>% 
  
  # 4. set dt format to match with start and end
  mutate(dt = as.POSIXct(dt, tz = "UTC")) %>%
  
  # Fill in any missing hours (missing rows get NA for other columns)
  complete(dt = seq(from = start_dt, to = end_dt, by = "1 hour")) %>%
  
  # Calculate the Sept 1 – Aug 31 season
  mutate(
    start_year = if_else(month(dt) >= 9, year(dt), year(dt) - 1),
    season = paste0(start_year, "-", start_year + 1)
  ) %>%
  select(-start_year) %>% 
  
  # sort by timestamp
  arrange(dt) %>%
  mutate(temp_filled = na.approx(air_temperature, na.rm = FALSE),
         temp_filled = round(temp_filled, 1),
         
         # 3. Extract Time as a formatted character string ("10:00:00")
         time = format(dt, "%H:%M:%S"),
         hour = format(dt, "%H")) %>% 
  filter(!is.na(obs_time))


penticton_airport <- penticton_airport %>%
  mutate(
    # 1. Convert string to POSIXct datetime (defaults to UTC)
    dt = ymd_hms(obs_time),
    
    # 2. Extract Date (returns a Date object: 2026-11-23)
    date = as_date(dt),
    
    # 3. Extract Time as a formatted character string ("10:00:00")
    time = format(dt, "%H:%M:%S"),
    hour = format(dt, "%H")) %>% 
  
  # 4. set dt format to match with start and end
  mutate(dt = as.POSIXct(dt, tz = "UTC")) %>%
  
  # Fill in any missing hours (missing rows get NA for other columns)
  complete(dt = seq(from = start_dt, to = end_dt, by = "1 hour")) %>%
  
  # Calculate the Sept 1 – Aug 31 season
  mutate(
    start_year = if_else(month(dt) >= 9, year(dt), year(dt) - 1),
    season = paste0(start_year, "-", start_year + 1)
  ) %>%
  select(-start_year) %>% 
  
  # sort by timestamp
  arrange(dt) %>%
  mutate(temp_filled = na.approx(air_temperature, na.rm = FALSE),
         temp_filled = round(temp_filled, 1),
         
         # 3. Extract Time as a formatted character string ("10:00:00")
         time = format(dt, "%H:%M:%S"),
         hour = format(dt, "%H")) %>% 
  filter(!is.na(obs_time))

summerland <- summerland %>%
  mutate(
    # 1. Convert string to POSIXct datetime (defaults to UTC)
    dt = ymd_hms(obs_time),
    
    # 2. Extract Date (returns a Date object: 2026-11-23)
    date = as_date(dt),
    
    # 3. Extract Time as a formatted character string ("10:00:00")
    time = format(dt, "%H:%M:%S"),
    hour = format(dt, "%H")) %>% 
  
  # 4. set dt format to match with start and end
  mutate(dt = as.POSIXct(dt, tz = "UTC")) %>%
  
  # Fill in any missing hours (missing rows get NA for other columns)
  complete(dt = seq(from = start_dt, to = end_dt, by = "1 hour")) %>%
  
  # Calculate the Sept 1 – Aug 31 season
  mutate(
    start_year = if_else(month(dt) >= 9, year(dt), year(dt) - 1),
    season = paste0(start_year, "-", start_year + 1)
  ) %>%
  select(-start_year) %>% 
  
  # sort by timestamp
  arrange(dt) %>%
  mutate(temp_filled = na.approx(air_temperature, na.rm = FALSE),
         temp_filled = round(temp_filled, 1),
         
         # 3. Extract Time as a formatted character string ("10:00:00")
         time = format(dt, "%H:%M:%S"),
         hour = format(dt, "%H")) %>% 
  filter(!is.na(obs_time))

##############################
# export cleaned weather data
##############################

write.csv(kelowna_airport, file = "./02_outputs/weather/kelowna_airport.csv", row.names = FALSE)
write.csv(kelowna_college, file = "./02_outputs/weather/kelowna_college.csv", row.names = FALSE)
write.csv(kelowna_ubco, file = "./02_outputs/weather/kelowna_ubco.csv", row.names = FALSE)
write.csv(osoyoos, file = "./02_outputs/weather/osoyoos.csv", row.names = FALSE)
write.csv(penticton_airport, file = "./02_outputs/weather/penticton_airport.csv", row.names = FALSE)
write.csv(summerland, file = "./02_outputs/weather/summerland.csv", row.names = FALSE)
