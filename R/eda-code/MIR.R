library(readxl)
library(dplyr)
library(lubridate)
library(tidyr) # For separate_rows()
library(here)

# Read the dataset
pcr_data <- read_excel(here("data", "raw-data", "virus_iso.xlsx"))

# Add week number based on the collection data
# This code ensures that the ISO 8601 standard is applied to the week number calculation
# Week 1 of any year is the week that contains January 4th, or equivalently, it's the week that contains the first Thursday of January.
# Weeks start on Monday and end on Sunday.
# The last week of the year, week 52 or 53, is the one that contains December 28th.
# we will use week_num to join tables
# the format will be YYYY-WW. Week 8 of 2019 will look like: 2019-08
pcr_data <- pcr_data %>%
  mutate(
    # Directly format the collection date into "YYYY-WW"
    week_num = paste0(year(`Collection Date`), "-", 
                      sprintf("%02d", isoweek(`Collection Date`)))
  )


# Correctly expand "Both" in Test Type, then adjust to "EEE" and "WNV"
pcr_data_expanded <- pcr_data %>%
  mutate(Test_Type_Expanded = case_when(
    `Test Type` == "Both" ~ "EEE&WNV", # Temporarily mark "Both" for special handling
    TRUE ~ `Test Type`
  )) %>%
  separate_rows(Test_Type_Expanded, sep = "&") %>%
  mutate(
    Year = year(`Collection Date`),
    Week = week(`Collection Date`),
    Submitted_for_Testing = `Submitted for Testing` == "Y",
    # Assuming positive results are marked as "Positive" in `Result`
    Is_Positive = Result == "Positive"
  )

# Save as RDS
saveRDS(pcr_data_expanded, here("data", "processed-data", "rds", "pcr_data_expanded.rds"))

# Save as CSV
write_csv(pcr_data_expanded, here("data", "processed-data", "pcr_data_expanded.csv"))

# Filter records where Submitted for Testing is "Y"
pcr_data_expanded_filtered <- pcr_data_expanded %>%
  filter(Submitted_for_Testing)

# Save as RDS
saveRDS(pcr_data_expanded_filtered, here("data", "processed-data", "rds", "pcr_data_expanded_filtered.rds"))

# Save as CSV
write_csv(pcr_data_expanded_filtered, here("data", "processed-data", "pcr_data_expanded_filtered.csv"))

# Calculate MIR for each Test Type category ("EEE", "WNV", and both considered in each)
summary_data <- pcr_data_expanded_filtered %>%
  group_by(Year, Week, Town, Result, Test_Type_Expanded) %>%
  summarise(
    Pools_Tested = sum(Submitted_for_Testing, na.rm = TRUE),
    Total_Tested = sum(`Pool Size`, na.rm = TRUE),
    Positive_Pools = sum(Is_Positive, na.rm = TRUE), # Directly use Is_Positive
    MIR = (Positive_Pools / Total_Tested) * 1000,
    .groups = 'drop'
  )
 
# Print the summary
print(summary_data)

# First, calculate the total number of tests submitted for testing by week_num
total_tests_by_week <- pcr_data %>%
  filter(`Submitted for Testing` == "Y") %>%
  group_by(week_num) %>%
  summarise(Total_Tested = sum(`Pool Size`, na.rm = TRUE), .groups = 'drop')

# Now, expand the data for detailed analysis as before
pcr_data_expanded <- pcr_data %>%
  mutate(Test_Type_Expanded = case_when(
    `Test Type` == "Both" ~ "EEE&WNV",
    TRUE ~ `Test Type`
  )) %>%
  separate_rows(Test_Type_Expanded, sep = "&") %>%
  mutate(
    Submitted_for_Testing = `Submitted for Testing` == "Y",
    Is_Positive = Result == "Positive"
  ) %>%
  filter(Submitted_for_Testing)

# Calculate MIR for each week_num across all towns and test types
MIR_by_week <- pcr_data_expanded %>%
  group_by(week_num) %>%
  summarise(
    Pools_Tested = sum(Submitted_for_Testing, na.rm = TRUE),
    Positive_Pools = sum(Is_Positive, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  left_join(total_tests_by_week, by = "week_num") %>%
  mutate(
    MIR = (Positive_Pools / Total_Tested) * 1000
  )

View(MIR_by_week)

library(dplyr)
library(stringr)

#calculate MIR for all sites by week
MIR_by_week <- pcr_data_expanded %>%
  group_by(week_num) %>%
  summarise(
    Pools_Tested = sum(Submitted_for_Testing, na.rm = TRUE),
    Positive_Pools = sum(Is_Positive, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  left_join(total_tests_by_week, by = "week_num") %>%
  mutate(
    MIR = (Positive_Pools / Total_Tested) * 1000,
    # Extract Year and Week from week_num
    Year = as.numeric(str_extract(week_num, "^[0-9]{4}")),
    Week = as.numeric(str_extract(week_num, "(?<=-)[0-9]{2}$")) # Extract two digits after the hyphen
  )

# Now MIR_by_week contains the Week and Year columns
print(MIR_by_week)


# create a heat map of the MIR values by week and year
library(ggplot2)
library(RColorBrewer)
library(dplyr)

# Filter the dataset for weeks 25 to 45
MIR_by_week_filtered <- MIR_by_week %>%
  mutate(Week = as.numeric(Week)) %>%  # Ensure that Week is numeric for filtering
  filter(Week >= 26 & Week <= 42)

# Adjusted heatmap creation code
MIR_by_week_heatmap <- ggplot(MIR_by_week_filtered, aes(x = factor(Week), y = factor(Year), fill = MIR)) +
  geom_tile(color = "white", size = 0.5) +  # Add borders to tiles for clarity
  scale_fill_gradientn(colors = brewer.pal(8, "Reds")) +  # Use a color palette from RColorBrewer
  scale_x_discrete(limits = as.character(26:42)) +  # Set discrete limits for weeks 25 to 45
  labs(
    title = "Weekly MIR from Week 26 to 42 from 2007 to 2023",
    x = "Week of the Year",
    y = "Year",
    fill = "MIR"
  ) +
  theme_minimal(base_size = 14) +  # Increase base font size for minimal theme
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),  # Rotate x-axis labels for better readability
    axis.title = element_text(size = 16),  # Increase size of axis titles
    plot.title = element_text(size = 20, hjust = 0.5),  # Increase size of plot title
    legend.title = element_text(size = 14),  # Adjust size of legend title
    legend.text = element_text(size = 12)  # Adjust size of legend text
  )

MIR_by_week_heatmap
# Save the heatmap as a PNG file
ggsave(here("results", "figures", "MIR_by_week_heatmap.png"), MIR_by_week_heatmap, width = 10, height = 8, units = "in")

