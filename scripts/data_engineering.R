# Load required packages
library(readr)      # for read_csv
library(dplyr)      # for mutate, case_when, across, na_if
library(stringr)    # for str_sub, str_detect
library(lubridate)  # for ymd_hms

# Read the CSV file
storm_data <- read_csv("StormEvents_details-ftp_v1.0_d2015_c20260323.csv")
#storm_data <- read_csv("weather.csv") #If you are using the one from github

damage_to_dollars <- function(x) {
  x <- str_trim(x)   # remove extra spaces (Chapter 5)
  case_when(
    x == "" | is.na(x) ~ NA_real_,
    str_detect(x, "K") ~ as.numeric(str_sub(x, 1, -2)) * 1000,
    str_detect(x, "M") ~ as.numeric(str_sub(x, 1, -2)) * 1000000,
    TRUE ~ as.numeric(x)
  )
}

# Apply conversions
storm_data <- storm_data %>%
  mutate(
    # Convert damage columns to numeric
    DAMAGE_PROPERTY = damage_to_dollars(DAMAGE_PROPERTY),
    DAMAGE_CROPS   = damage_to_dollars(DAMAGE_CROPS),
    
    # Parse dates (format "DD-MMM-YY HH:MM:SS")
    BEGIN_DATE_TIME = ymd_hms(BEGIN_DATE_TIME),
    END_DATE_TIME   = ymd_hms(END_DATE_TIME),
    
    # Convert numeric columns (suppress warnings from blanks)
    INJURIES_DIRECT   = as.numeric(INJURIES_DIRECT),
    INJURIES_INDIRECT = as.numeric(INJURIES_INDIRECT),
    DEATHS_DIRECT     = as.numeric(DEATHS_DIRECT),
    DEATHS_INDIRECT   = as.numeric(DEATHS_INDIRECT),
    BEGIN_LAT         = as.numeric(BEGIN_LAT),
    BEGIN_LON         = as.numeric(BEGIN_LON),
    END_LAT           = as.numeric(END_LAT),
    END_LON           = as.numeric(END_LON),
    MAGNITUDE         = as.numeric(MAGNITUDE)
  ) %>%
  # Replace empty strings with NA in all character columns
  mutate(across(where(is.character), ~ na_if(str_trim(.), "")))

# View the result
glimpse(storm_data)

write_csv(storm_data, "storm_events_cleaned.csv")
