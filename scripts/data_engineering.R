# Load required packages
library(readr)      # for read_csv
library(dplyr)      # for mutate, case_when, across, na_if
library(stringr)    # for str_sub, str_detect
library(lubridate)  # for ymd_hms


# Read the CSV file
storm_data <- read_csv("data/weather.csv")

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

write_csv(storm_data, "data/storm_events_cleaned.csv")

# This is the Flights dataset 2015 flight delays from Kaggle
flights <- read.csv("data/flights.csv")

# Supporting lookup tables for flights
airlines <- read.csv("data/airlines.csv")
airports <- read.csv("data/airports.csv")

# This is the Weather dataset NOAA 2015 storm events 
storm_data <- read.csv("data/storm_events_cleaned.csv")

# This is the Tweets dataset airline sentiment tweets 
twitter_data <- read.csv("data/Tweets-1.csv")

#check of all dataset dimensions
cat("Flights rows:", nrow(flights), "\n")
cat("Storm events rows:", nrow(storm_data), "\n")
cat("Tweets rows:", nrow(twitter_data), "\n")
cat("Airlines rows:", nrow(airlines), "\n")
cat("Airports rows:", nrow(airports), "\n")



#Full airline names by joining with airlines lookup table
flights_clean <- flights %>%
  left_join(airlines, by = c("AIRLINE" = "IATA_CODE")) %>%
  rename(AIRLINE_NAME = AIRLINE.y) %>%
  # State info by joining airports on origin airport code
  left_join(airports %>% select(IATA_CODE, STATE),
            by = c("ORIGIN_AIRPORT" = "IATA_CODE")) %>%
  rename(ORIGIN_STATE = STATE) %>%
  # Removed rows with no delay info
  filter(!is.na(DEPARTURE_DELAY)) %>%
  # Replaced NA delay values with 0
  # NA in delay columns means no delay occurred
  mutate(
    DEPARTURE_DELAY = ifelse(is.na(DEPARTURE_DELAY), 0, DEPARTURE_DELAY),
    ARRIVAL_DELAY = ifelse(is.na(ARRIVAL_DELAY), 0, ARRIVAL_DELAY),
    WEATHER_DELAY = ifelse(is.na(WEATHER_DELAY), 0, WEATHER_DELAY),
    AIR_SYSTEM_DELAY = ifelse(is.na(AIR_SYSTEM_DELAY), 0, AIR_SYSTEM_DELAY),
    AIRLINE_DELAY = ifelse(is.na(AIRLINE_DELAY), 0, AIRLINE_DELAY),
    LATE_AIRCRAFT_DELAY = ifelse(is.na(LATE_AIRCRAFT_DELAY), 0, LATE_AIRCRAFT_DELAY),
    SECURITY_DELAY = ifelse(is.na(SECURITY_DELAY), 0, SECURITY_DELAY)
  ) %>%
  # Created a delayed flag and delayed means more than 15 minutes
  mutate(IS_DELAYED = ifelse(DEPARTURE_DELAY > 15, 1, 0))

# Selected only the columns we need for analysis
storm_clean <- storm_data %>%
  select(STATE, MONTH_NAME, EVENT_TYPE,
         DAMAGE_PROPERTY, DAMAGE_CROPS,
         INJURIES_DIRECT, DEATHS_DIRECT,
         BEGIN_DATE_TIME) %>%
  # Removed rows with no state
  filter(!is.na(STATE)) %>%
  # Standardized state to uppercase to match flights data
  mutate(STATE = toupper(STATE)) %>%
  # Replaced NA damage values with median 
  # Median is better than mean here because damage values
  # are heavily skewed by large disaster events
  mutate(
    DAMAGE_PROPERTY = ifelse(is.na(DAMAGE_PROPERTY),
                             median(DAMAGE_PROPERTY, na.rm = TRUE),
                             DAMAGE_PROPERTY),
    DAMAGE_CROPS = ifelse(is.na(DAMAGE_CROPS),
                          median(DAMAGE_CROPS, na.rm = TRUE),
                          DAMAGE_CROPS)
  )

# Shows structure of cleaned weather data
glimpse(storm_clean)
