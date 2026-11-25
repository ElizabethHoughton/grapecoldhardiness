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
library(randomForest)
library(ggpubr)

##############################
# pull in LTE data
##############################

LTE <- read.csv("./02_outputs/LTE_weather.csv") %>% 
  mutate(date = as.Date(date),
         season = as.factor(season),
         location = as.factor(location),
         variety = as.factor(variety),
         weather_station = as.factor(weather_station))

##############################
# Add jday_cum
##############################

df <- tibble(
  date = seq(as.Date("2012-09-01"), as.Date("2026-09-01"), by = "day")
) %>%
  mutate(
    # Identify the starting year of the Sept 1 - Aug 31 season
    season_start_year = if_else(month(date) >= 9, year(date), year(date) - 1),
    season_start = make_date(season_start_year, 9, 1),
    
    # Calculate cumulative day (Sept 1 = 1)
    jday_cum = as.integer(date - season_start) + 1
  ) %>%
  select(date, jday_cum)

LTE <- merge(LTE, df, by = c("date"))

##############################
# Exploration and removal
##############################

# firslt, we will remove orchard/varieties will little to no sampling
# and anything with 1 to 3 yr vine testing



LTE_split <- LTE %>%
  unite(
    col = "variety_location_season",      # Name of the new column
    c(variety, location, season),     # The 3 columns to merge
    sep = "_",               # Separator (use "" for no space)
    remove = FALSE,          # Keep original columns (TRUE drops them)
    na.rm = TRUE             # Ignore NAs instead of pasting "NA"
  ) %>% 
  unite(
    col = "variety_location",      # Name of the new column
    c(variety, location),     # The 3 columns to merge
    sep = "_",               # Separator (use "" for no space)
    remove = FALSE,          # Keep original columns (TRUE drops them)
    na.rm = TRUE             # Ignore NAs instead of pasting "NA"
  ) %>% 
  drop_na(variety)
  
variety <- LTE_split %>%
  count(variety, sort = TRUE)


# variety   n
# 1                  merlot 967
# 2              chardonnay 753
# 3              pinot_gris 690
# 4         sauvignon_blanc 598
# 5                  shiraz 585
# 6      cabernet_sauvignon 577
# 7                riesling 548
# 8          cabernet_franc 539
# 9              pinot_noir 489
# 10         gewurztraminer 289
# 11            pinot_blanc 226
# 12               viognier 206
# 13                 malbec 159
# 14              zinfandel  79
# 15             gamay_noir  40
# 16          shiraz_cl.174  40
# 17            tempranillo  37
# 18              marquette  24
# 19     cabernet_franc_3yr  22
# 20     cabernet_franc_2yr  19
# 21                  syrah  18
# 22 cabernet_sauvignon_2yr  16
# 23          zinfandel_2yr  16
# 24           chenin_blanc  13
# 25           grenache_1yr  10
# 26          mourvedre_1yr  10
# 27 cabernet_sauvignon_1yr   9
# 28 cabernet_sauvignon_3yr   8
# 29           grenache_2yr   8
# 30          mourvedre_2yr   8
# 31          pinot_meunier   8
# 32          zinfandel_3yr   8
# 33     cabernet_franc_1yr   7
# 34        frontenac_blanc   6
# 35          zinfandel_1yr   6
# 36                 muscat   4
# 37           petit_verdot   4
# 38              teroldego   4


# remove varieties with minimal data overall

LTE_split <- LTE_split  %>%
  subset(!variety %in% c("marquette", "cabernet_franc_3yr", "cabernet_franc_2yr", "syrah",
                         "cabernet_sauvignon_2yr", "zinfandel_2yr", "chenin_blanc", "grenache_1yr",
                         "mourvedre_1yr", "cabernet_sauvignon_1yr", "cabernet_sauvignon_3yr", 
                         "grenache_2yr", "mourvedre_2yr", "pinot_meunier", "zinfandel_3yr", "cabernet_franc_1yr", 
                         "frontenac_blanc", "zinfandel_1yr", "muscat", "petit_verdot", "teroldego"))

variety <- LTE_split %>%
  count(variety, sort = TRUE)

# variety   n
# 1              merlot 967
# 2          chardonnay 753
# 3          pinot_gris 690
# 4     sauvignon_blanc 598
# 5              shiraz 585
# 6  cabernet_sauvignon 577
# 7            riesling 548
# 8      cabernet_franc 539
# 9          pinot_noir 489
# 10     gewurztraminer 289
# 11        pinot_blanc 226
# 12           viognier 206
# 13             malbec 159
# 14          zinfandel  79
# 15         gamay_noir  40
# 16      shiraz_cl.174  40
# 17        tempranillo  37

variety_location_season <- LTE_split %>%
  count(variety_location_season, sort = TRUE)

# only keep seasons with at least 3 measurements per vineyard

LTE_split <- LTE_split  %>%
  subset(!variety_location_season %in% c("viognier_lake_country_2024-2025"))

variety_location <- LTE_split %>%
  count(variety_location, sort = TRUE)

##############################
# 70 % model 30 % test split
##############################

# group by variety_location and randomly select 70/30 of the seasons

set.seed(123) # Ensures reproducibility

# add your quadratic relationship
LTE_split <- LTE_split %>% 
  mutate(cum_chill_portions_sqr = cum_chill_portions^2)

# 1. Get unique variety_location + season pairs and assign 70% to train
season_splits <- LTE_split %>%
  distinct(variety_location, season) %>%
  group_by(variety_location) %>%
  mutate(
    # Shuffle seasons randomly within each variety_location
    rand_order = sample(row_number()),
    n_seasons = n(),
    # Keep at least 1 season in train; otherwise take ~70%
    n_train = pmax(1, round(n_seasons * 0.7)),
    split = if_else(rand_order <= n_train, "train", "test")
  ) %>%
  ungroup() %>%
  select(variety_location, season, split)

# 2. Join the split labels back to your full dataset
df_split <- LTE_split %>%
  left_join(season_splits, by = c("variety_location", "season"))

# 3. Separate into train and test dataframes
train_df <- df_split %>% filter(split == "train") %>% select(-split)
test_df  <- df_split %>% filter(split == "test") %>% select(-split)


##############################
# do a quick little RF model
##############################

rf_model_1 <- randomForest(LTE ~ temp_max_lag1 + temp_max_lag2 + temp_max_lag3 + temp_max_lag4 + temp_max_lag5 + temp_max_lag6 + temp_max_lag7 + daylength_hrs + cum_chill_portions + cum_chill_portions_sqr + variety, data = train_df, ntree = 500)

predictions <- predict(rf_model_1, train_df)

predictions <- cbind(predictions, train_df) 

# 2. Calculate RMSE for each group
df_rmse <- predictions %>%
  group_by(variety) %>%
  summarise(rmse = sqrt(mean((LTE - predictions)^2)),
            y_pos = max(LTE)+4) %>%
  mutate(label = paste0("RMSE (", variety, "): ", round(rmse, 2)))

# --- 3. MODELLING 5 VISUALIZATION ---

# train_data
ggplot(predictions, aes(x = predictions, y = LTE, color = variety, shape = location)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +  # Add regression lines
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") + # 1:1 line
  labs(title = "1:1 Regression by Variety", x = "Observed", y = "Predicted") +
  coord_cartesian(xlim = c(-30, -5), ylim = c(-30, -5)) +
  theme_minimal()

ggplot(predictions, aes(x = predictions, y = LTE, color = variety)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +  # Add regression lines
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") + # 1:1 line
  labs(title = "1:1 Regression by Variety", x = "Observed", y = "Predicted") +
  coord_cartesian(xlim = c(-30, -5), ylim = c(-30, -5)) +
  theme_minimal()

ggplot(predictions, aes(x = predictions, y = LTE, color = variety)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +  # Add regression lines
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           label.y.npc = "top") +
  geom_text(data = df_rmse, 
            aes(x = -20, y = y_pos, label = label), # Adjust position
            color = "black", hjust = 0) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") + # 1:1 line
  labs(title = "1:1 Regression by Variety", x = "Observed", y = "Predicted") +
  coord_cartesian(xlim = c(-30, -5), ylim = c(-30, -5)) +
  theme_minimal()

ggplot(predictions) +
  geom_line(aes(x = jday_cum, y = predictions, color = variety)) +
  geom_point(aes(x = jday_cum, y = LTE, color = variety)) +
  coord_cartesian(ylim = c(-35, -5)) +
  facet_wrap(~ season + location) + 
  # facet_wrap(~ season + variety + location) + 
  geom_vline(xintercept = 181) # at approximately 10hrs day length in spring

# test_data

predictions <- predict(rf_model_1, test_df)

predictions <- cbind(predictions, test_df) 

# calculate RMSE for each group
df_rmse <- predictions %>%
  group_by(variety) %>%
  summarise(rmse = sqrt(mean((LTE - predictions)^2)),
            y_pos = max(LTE)+4) %>%
  mutate(label = paste0("RMSE (", variety, "): ", round(rmse, 2)))

ggplot(predictions, aes(x = predictions, y = LTE, color = variety, shape = location)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +  # Add regression lines
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") + # 1:1 line
  labs(title = "1:1 Regression by Variety", x = "Observed", y = "Predicted") +
  coord_cartesian(xlim = c(-30, -5), ylim = c(-30, -5)) +
  theme_minimal()

ggplot(predictions, aes(x = predictions, y = LTE, color = variety)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +  # Add regression lines
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") + # 1:1 line
  labs(title = "1:1 Regression by Variety", x = "Observed", y = "Predicted") +
  coord_cartesian(xlim = c(-30, -5), ylim = c(-30, -5)) +
  theme_minimal()

ggplot(predictions, aes(x = predictions, y = LTE, color = variety)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +  # Add regression lines
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           label.y.npc = "top") +
  geom_text(data = df_rmse, 
            aes(x = -20, y = y_pos, label = label), # Adjust position
            color = "black", hjust = 0) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") + # 1:1 line
  labs(title = "1:1 Regression by Variety", x = "Observed", y = "Predicted") +
  coord_cartesian(xlim = c(-30, -5), ylim = c(-30, -5)) +
  theme_minimal()

ggplot(predictions) +
  geom_line(aes(x = jday_cum, y = predictions, color = variety)) +
  geom_point(aes(x = jday_cum, y = LTE, color = variety)) +
  coord_cartesian(ylim = c(-35, -5)) +
  facet_wrap(~ season + location) + 
  # facet_wrap(~ season + variety + location) + 
  geom_vline(xintercept = 181) # at approximately 10hrs day length in spring
