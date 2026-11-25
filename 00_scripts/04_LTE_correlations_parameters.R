################################################################################
# Exploring LTE relationships with dailyweather variables
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

library(tidyverse)
library(ggplot2)
#library(GGally)
library(broom)
library(lme4)
library(lmerTest)
library(performance)
library(mgcv)

##############################
# pull in LTE data
##############################

LTE <- read.csv("./02_outputs/LTE50.csv") %>% 
  select(-season)

location <- LTE %>%
  count(location, sort = TRUE)

##############################
# pull in weather data
##############################

kelowna_airport <- read.csv("./02_outputs/daily_weather_parameters/kelowna_airport.csv")
kelowna_college <- read.csv("./02_outputs/daily_weather_parameters/kelowna_college.csv") 
#kelowna_ubco <- read.csv("./02_outputs/daily_weather_parameters/kelowna_ubco.csv") 
osoyoos <- read.csv("./02_outputs/daily_weather_parameters/osoyoos.csv") 
penticton_airport <- read.csv("./02_outputs/daily_weather_parameters/penticton_airport.csv") 
summerland <- read.csv("./02_outputs/daily_weather_parameters/summerland.csv") 

##############################
# merge df based on location
##############################

kelowna_airport <- kelowna_airport %>%
  left_join(
    LTE %>% filter(location %in% c("north_kelowna", "lake_country")),
    by = "date"
  ) %>% 
  mutate(weather_station = "kelowna_airport") %>% 
  mutate(date = as.Date(date),
         season = as.factor(season),
         location = as.factor(location),
         variety = as.factor(variety),
         weather_station = as.factor(weather_station))

kelowna_college <- kelowna_college %>%
  left_join(
    LTE %>% filter(location %in% c("kelowna", "south_kelowna", "east_kelowna", "west_kelowna")),
    by = "date"
  ) %>% 
  mutate(weather_station = "kelowna_college") %>% 
  mutate(date = as.Date(date),
         season = as.factor(season),
         location = as.factor(location),
         variety = as.factor(variety),
         weather_station = as.factor(weather_station))

# starts in 2013-03-14; decided to simply use kelowna_airport instead
# kelowna_ubco <- kelowna_ubco %>%
#   left_join(
#     LTE %>% filter(location == "north_kelowna"),
#     by = "date"
#   ) %>% 
# mutate(weather_station = "kelowna_ubco")

osoyoos <- osoyoos %>%
  left_join(
    LTE %>% filter(location %in% c("osoyoos_north", "osoyoos_west", "osoyoos_northeast", "osoyoos_southeast",
                                   "oliver", "oliver_north", "oliver_west", "oliver_south", "oliver_east",
                                   "black_sage", "similkameen")),
    by = "date"
  ) %>% 
  mutate(weather_station = "osoyoos") %>% 
  mutate(date = as.Date(date),
         season = as.factor(season),
         location = as.factor(location),
         variety = as.factor(variety),
         weather_station = as.factor(weather_station))

penticton_airport <- penticton_airport %>%
  left_join(
    LTE %>% filter(location %in% c("okanagan_falls_east", "okanagan_falls_west",
                                   "kaleden", "naramata_bench")),
    by = "date"
  ) %>% 
  mutate(weather_station = "penticton_airport") %>% 
  mutate(date = as.Date(date),
         season = as.factor(season),
         location = as.factor(location),
         variety = as.factor(variety),
         weather_station = as.factor(weather_station))

summerland <- summerland %>%
  left_join(
    LTE %>% filter(location %in% c("summerland", "peachland")),
    by = "date"
  ) %>% 
  mutate(weather_station = "summerland") %>% 
  mutate(date = as.Date(date),
         season = as.factor(season),
         location = as.factor(location),
         variety = as.factor(variety),
         weather_station = as.factor(weather_station))


# make one master df

LTE_weather <- rbind(kelowna_airport, kelowna_college, osoyoos, penticton_airport, summerland)

LTE_weather <- LTE_weather %>% 
  subset(season != "2026-2027")


##############################
# export LTE_weather data
##############################

write.csv(LTE_weather, file = "./02_outputs/LTE_weather.csv", row.names = FALSE)

##############################
# data exploration
##############################

# Continuous variables
continuous_vars <- c(
  "LTE",
  "cum_chill_portions",
  "jday",
  "temp_mean",
  "temp_mean_lag1",
  "temp_mean_lag2",
  "temp_mean_lag3",
  "temp_mean_lag4",
  "temp_mean_lag5",
  "temp_mean_lag6",
  "temp_mean_lag7",
  "temp_min",
  "temp_min_lag1",
  "temp_min_lag2",
  "temp_min_lag3",
  "temp_min_lag4",
  "temp_min_lag5",
  "temp_min_lag6",
  "temp_min_lag7",
  "temp_max",
  "temp_max_lag1",
  "temp_max_lag2",
  "temp_max_lag3",
  "temp_max_lag4",
  "temp_max_lag5",
  "temp_max_lag6",
  "temp_max_lag7",
  "temp_avg",
  "daylength_hrs"
)

# Convert to long format
LTE_distribution <- LTE_weather %>%
  pivot_longer(
    cols = all_of(continuous_vars),
    names_to = "parameter",
    values_to = "value"
  )


##############
# Plots
##############

# Plot distributions of continuous variables
ggplot(
  LTE_distribution,
  aes(x = value)
) +
  geom_histogram(
    bins = 50,
    fill = "steelblue",
    color = "steelblue"
  ) +
  facet_wrap(
    ~ parameter,
    scales = "free",
    ncol = 4
  ) +
  theme_bw() +
  labs(
    x = "Value",
    y = "Frequency",
    title = "Distribution of LTE and environmental parameters"
  )

# compare distributions among location and varieties
ggplot(
  LTE_weather,
  aes(x = location, y = LTE)
) +
  geom_boxplot(
    aes(fill = location),
    alpha = 0.7,
    outlier.alpha = 0.3
  ) +
  theme_bw() +
  theme(
    legend.position = "none",
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    )
  ) +
  labs(
    x = "Location",
    y = "LTE",
    title = "Distribution of LTE across locations"
  )

ggplot(
  LTE_weather,
  aes(x = variety, y = LTE, fill = variety)
) +
  geom_boxplot(alpha = 0.7) +
  theme_bw() +
  theme(legend.position = "none") +
  labs(
    x = "Variety",
    y = "LTE",
    title = "Distribution of LTE across varieties"
  )


# interaction between location and variety 

ggplot(
  LTE_weather,
  aes(
    x = location,
    y = LTE,
    fill = variety
  )
) +
  geom_boxplot(
    position = position_dodge(width = 0.8)
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    )
  ) +
  labs(
    x = "Location",
    y = "LTE",
    fill = "Variety",
    title = "LTE distributions by location and variety"
  )

# look at more things

LTE_long <- LTE_weather %>%
  pivot_longer(
    cols = c(cum_chill_portions, temp_mean, temp_min, temp_max, temp_avg, jday,
             temp_min_lag1, temp_min_lag2, temp_min_lag3, temp_min_lag4, temp_min_lag5, temp_min_lag6,temp_min_lag7,
             temp_mean_lag1, temp_mean_lag2, temp_mean_lag3, temp_mean_lag4, temp_mean_lag5, temp_mean_lag6,temp_mean_lag7,
             temp_max_lag1, temp_max_lag2, temp_max_lag3, temp_max_lag4, temp_max_lag5, temp_max_lag6, temp_max_lag7,
             daylength_hrs),
    names_to = "parameter",
    values_to = "value"
  )

ggplot(LTE_long, aes(x = value, y = LTE, color = variety, shape = location)) +
  geom_point(size = 2.5, alpha = 0.8) +
  facet_wrap(~ parameter, scales = "free_x") +
  labs(
    title = "LTE vs. Environmental Parameters",
    x = "Parameter Value",
    y = "LTE",
    color = "Variety",
    shape = "Location"
  ) +
  theme_bw() +
  theme(
    strip.background = element_rect(fill = "grey90"),
    strip.text = element_text(face = "bold")
  )

# look at correlation
predictors <- LTE_weather[, sapply(LTE_weather, is.numeric) & names(LTE_weather) != "LTE"]

# Compute Pearson correlations with LTE
cor_values <- cor(predictors, LTE_weather$LTE, use = "pairwise.complete.obs", method = "pearson")

# Convert to a clean data frame
correlation <- data.frame(
  parameter = rownames(cor_values),
  r = as.numeric(cor_values)
)


ggplot(LTE_weather, aes(x = date, y = LTE, colour = variety)) +
  geom_point(alpha = 0.6, size = 2) +
  geom_line(aes(group = interaction(variety, season)),
            alpha = 0.5) +
  facet_wrap(~ weather_station, scales = "free_x") +
  labs(
    title = "LTE over time by weather station",
    x = "Date",
    y = "LTE",
    colour = "Variety"
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(face = "bold"),
    legend.position = "bottom"
  )

ggplot(LTE_weather, aes(
  x = date,
  y = LTE,
  colour = variety
)) +
  geom_point(alpha = 0.7, size = 2) +
  geom_line(aes(group = interaction(variety, location)),
            alpha = 0.4) +
  facet_grid(weather_station ~ season, scales = "free_x") +
  labs(
    title = "LTE by season, date, and weather station",
    x = "Date",
    y = "LTE",
    colour = "Variety"
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(face = "bold"),
    legend.position = "bottom"
  )


