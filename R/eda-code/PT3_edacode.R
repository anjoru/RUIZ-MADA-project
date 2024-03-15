## ---- packages --------
#load needed packages. make sure they are installed.
library(here) #for data loading/saving
library(dplyr)
library(skimr)
library(ggplot2)
library(tidyverse)
library(lubridate)
library(readr)
library(forecast)
library(tsibble) # For time series data wrangling
library(feasts) # For decomposing and visualizing time series

pcr <- read_csv(here("data", "processed-data", "pcr_data_expanded_filtered.csv"))
wx_selected <- read_csv(here("data", "processed-data", "wx_selected.csv"))
# Assuming wx_selected and pcr_data_expanded_filtered are already loaded

# Step 2: Calculate the average TMAX for April, May, and June separately
avg_temp_by_month <- wx_selected %>%
  filter(month(DATE) %in% c(4, 5, 6)) %>%
  group_by(Year = year(DATE), Month = month(DATE)) %>%
  summarise(Avg_TMAX = mean(TMAX, na.rm = TRUE), .groups = 'drop')

# Pivot the average temperatures into separate columns for each month
avg_temp_by_month_wide <- avg_temp_by_month %>%
  pivot_wider(names_from = Month, values_from = Avg_TMAX, names_prefix = "Avg_TMAX_M") %>%
  # Ensure all years are present even if some data might be missing
  complete(Year, fill = list(Avg_TMAX_M4 = NA, Avg_TMAX_M5 = NA, Avg_TMAX_M6 = NA))

# Step 4: Identify the first positive test date each year in mosquito_data
first_positive_dates <- pcr_data_expanded_filtered %>%
  filter(Result == "Positive") %>%
  group_by(Year = year(`Collection Date`)) %>%
  summarise(First_Positive_Date = min(`Collection Date`), .groups = 'drop')

# Step 5: Merge the datasets on Year
combined_data_firstpos <- left_join(first_positive_dates, avg_temp_by_month_wide, by = "Year")

# Step 6: Convert First_Positive_Date to a numerical value (e.g., day of the year)
combined_data_firstpos <- combined_data_firstpos %>%
  mutate(First_Positive_DOY = yday(First_Positive_Date))

# Explore the relationship via linear regression with Avg_TMAX_M4, Avg_TMAX_M5, Avg_TMAX_M6 as predictors
model <- lm(First_Positive_DOY ~ Avg_TMAX_M4 + Avg_TMAX_M5 + Avg_TMAX_M6, data = combined_data_firstpos)
summary(model)

#model 3
wx_selected <- read_csv(here("data", "processed-data", "wx_selected.csv"))

# Calculate average TMAX and total PRCP for April, May, and June separately
avg_temp_and_prcp <- wx_selected %>%
  filter(month(DATE) %in% c(4, 5, 6)) %>%
  group_by(Year = year(DATE), Month = month(DATE)) %>%
  summarise(
    Avg_TMAX = mean(TMAX, na.rm = TRUE),
    Total_PRCP = sum(PRCP, na.rm = TRUE),
    .groups = 'drop'
  )

# Continue from the summarise step
wide_data <- avg_temp_and_prcp %>%
  # Convert to a long format to distinguish between Avg_TMAX and Total_PRCP
  pivot_longer(cols = c(Avg_TMAX, Total_PRCP), names_to = "Measure", values_to = "Value") %>%
  # Create a combined name for later pivot_wider
  unite("NewName", Measure, Month, sep = "_M") %>%
  # Pivot wider with a single names_prefix
  pivot_wider(names_from = NewName, values_from = Value) %>%
  complete(Year, fill = list(Avg_TMAX_M4 = NA, Avg_TMAX_M5 = NA, Avg_TMAX_M6 = NA, Total_PRCP_M4 = NA, Total_PRCP_M5 = NA, Total_PRCP_M6 = NA))

# Load and prepare mosquito data
# Replace `pcr_data_expanded_filtered` with your actual mosquito data variable
first_positive_dates <- pcr_data_expanded_filtered %>%
  filter(Result == "Positive") %>%
  group_by(Year = year(`Collection Date`)) %>%
  summarise(First_Positive_Date = min(`Collection Date`), .groups = 'drop')

# Merge the datasets on Year
combined_data <- left_join(first_positive_dates, wide_data, by = "Year")

# Convert First_Positive_Date to a numerical value (e.g., day of the year)
combined_data <- combined_data %>%
  mutate(First_Positive_DOY = yday(First_Positive_Date))

# Explore the relationship via linear regression
model <- lm(First_Positive_DOY ~ Total_PRCP_M6, data = combined_data)
summary(model)
