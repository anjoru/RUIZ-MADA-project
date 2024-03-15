library(here)
library(dplyr)
library(lubridate)
library(readr)


# Load the PCR data set which includes information on mosquito pools tested for viruses, 
# results of those tests, and the date of collection.
pcr <- read_csv(here("data", "processed-data", "pcr_data_expanded_filtered.csv"))

# Load the weather data set which includes daily maximum temperatures (TMAX) and other weather variables.
wx_selected <- read_csv(here("data", "processed-data", "wx_selected.csv"))

# Aggregate the weather data by week to calculate the average maximum temperature for each week.
# This simplifies the daily temperature data into a more manageable weekly summary,
# making it easier to correlate with weekly mosquito test results.
wx_selected <- wx_selected %>%
  group_by(week_num) %>%
  summarise(Avg_TMAX = mean(TMAX, na.rm = TRUE)) %>%
  ungroup()

# Create a lagged version of the average weekly maximum temperature to explore the potential delayed effect of temperature on mosquito test results.
# The lag function shifts the temperature data by one week, assuming that the impact of temperature on virus transmission might not be immediate.
weather_weekly <- wx_selected %>%
  arrange(week_num) %>%
  mutate(Lagged_Avg_TMAX = lag(Avg_TMAX, 1))

# Aggregate the PCR data by week to determine if there was at least one positive result in each week.
# This transformation reduces the detailed test results into a weekly summary,
# indicating the presence or absence of positive tests each week.
pcr_weekly <- pcr %>%
  group_by(week_num) %>%
  summarise(Positive_Result = max(as.integer(Is_Positive), na.rm = TRUE))

# Merge the aggregated PCR data with the lagged weather data by matching their week numbers.
# This combined data set enables the analysis of the relationship between weekly weather conditions and mosquito test results.
combined_wx_pcr <- merge(pcr_weekly, weather_weekly, by = "week_num")

# Fit a logistic regression model to examine the influence of lagged average weekly maximum temperature on the likelihood of observing at least one positive mosquito test result in a week.
# The binomial family specifies that the dependent variable (Positive_Result) is binary,
# making logistic regression an appropriate modeling choice for this binary outcome.
logistic_model_temp <- glm(Positive_Result ~ Lagged_Avg_TMAX, family = binomial, data = combined_wx_pcr)

# Output the summary of the logistic regression model to review the estimated coefficients, their statistical significance, and the overall model fit.
# This summary provides insights into the relationship between temperature and the probability of positive mosquito test results.
summary(logistic_model_temp)

