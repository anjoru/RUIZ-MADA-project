# Load necessary libraries for data manipulation, date handling, and file path management
library(lubridate)
library(here)
library(dplyr)

# Load the datasets:
# MIR_by_week: Contains information on mosquito pool tests, including the week number and MIR.
# wx_selected: Weather data with daily maximum temperature (TMAX) and precipitation (PRCP).
# mosquito_wide: Another dataset related to mosquito data, although it's loaded, it's not used in the subsequent analysis.
MIR_by_week <- read_csv(here("data", "processed-data", "MIR_by_week.csv"))
wx_selected <- read_csv(here("data", "processed-data", "wx_selected.csv"))
mosquito_wide <- read_csv(here("data", "processed-data", "mosquito_wide.csv"))

# Aggregate weather data by week:
# Calculate the average maximum temperature (Avg_TMAX) and total precipitation (Total_PRCP) for each week.
# This simplification allows for weekly comparisons with the MIR data.
wx_aggregated <- wx_selected %>%
  group_by(week_num) %>%
  summarise(Avg_TMAX = mean(TMAX, na.rm = TRUE),
            Total_PRCP = sum(PRCP, na.rm = TRUE),
            .groups = 'drop')

# Adjust for a one-week lag in weather data:
# Create a new column (Lagged_week_num) that represents each week's preceding week. 
# This adjustment accounts for the delayed effect weather might have on MIR.
wx_aggregated <- wx_aggregated %>%
  mutate(
    Lagged_week_num = paste0(
      if_else(as.integer(substr(week_num, 6, 7)) == 1, as.integer(substr(week_num, 1, 4)) - 1, as.integer(substr(week_num, 1, 4))),
      "-",
      sprintf("%02d", if_else(as.integer(substr(week_num, 6, 7)) == 1, 53, as.integer(substr(week_num, 6, 7)) - 1))
    )
  )

# Combine the MIR and lag-adjusted weather data:
# Merge MIR data with weather data based on the adjusted week number, aligning MIR observations with the prior week's weather conditions.
combined_data_mir <- MIR_by_week %>%
  left_join(wx_aggregated, by = c("week_num" = "Lagged_week_num"))

# Fit a linear model to analyze the relationship between MIR and weather conditions:
# Model MIR as a function of the average maximum temperature (Avg_TMAX) and total precipitation (Total_PRCP) from the previous week.
model <- lm(MIR ~ Avg_TMAX + Total_PRCP, data = combined_data_mir)

# Display a summary of the model to review coefficients, significance levels, and overall model fit.
# This summary helps understand how weekly weather conditions (from the previous week) influence the MIR of mosquito pools.
summary(model)
