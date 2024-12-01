#### Preamble ####
# Purpose: Tests the structure and validity of the simulated data files.
# Author: Haowei Fan
# Date: November 28, 2024
# Contact: haowei.fan@mail.utoronto.ca
# License: Open Government License - Toronto
# Pre-requisites: 00-simulate_data.R must have been run.
# Any other information needed? Make sure you are in the `Bikeshare-Forecast.Rproj`.
# The code was completed under the guidance of ChatGPT-4, and the detailed interaction process can be found in the path：`other/llm_usage/`



#### Workspace setup ####
set.seed(912)
# List of necessary packages
packages <- c("tidyverse", "dplyr", "arrow")
# Install missing packages
missing_packages <- packages[!(packages %in% installed.packages()[, "Package"])]
install.packages(missing_packages)
# Load packages
lapply(packages, library, character.only = TRUE)

#### Read data ####
file_paths <- c("data/00-simulated_data/start_simulation.parquet", "data/00-simulated_data/stop_simulation.parquet")

test_dataset <- function(df) {
  
  # 1. Test if the data was successfully loaded
  if (exists("df")) {
    message("Test Passed: The dataset was successfully loaded.")
  } else {
    stop("Test Failed: The dataset could not be loaded.")
  }
  
  # 2. Test if there are any missing values in the dataset
  if (sum(is.na(df)) == 0) {
    message("Test Passed: No missing values found in the dataset.")
  } else {
    stop("Test Failed: Missing values detected in the dataset.")
  }
  
  # 3. Test if 'time' is within the range between 2017-01-01 00:00 and 2024-09-30 24:00
  time_start <- as.POSIXct("2016-12-31 24:00:00", format="%Y-%m-%d %H:%M:%S", tz="UTC")
  time_end <- as.POSIXct("2024-10-01 00:00:00", format="%Y-%m-%d %H:%M:%S", tz="UTC")
  df$time <- as.POSIXct(df$time, format="%Y-%m-%d %H:%M:%S", tz="UTC")
  if (all(df$time >= time_start & df$time <= time_end)) {
    message("Test Passed: All time values are within the specified range.")
  } else {
    stop("Test Failed: Some time values are outside the specified range.")
  }
  
  # 4. Test if there are any duplicate rows in the dataset
  if (nrow(df) == nrow(unique(df))) {
    message("Test Passed: No duplicate rows found in the dataset.")
  } else {
    stop("Test Failed: Duplicate rows detected in the dataset.")
  }
  
  # 5. Test if 'count' has any negative values
  if (all(df$count >= 0)) {
    message("Test Passed: No negative values found in 'count'.")
  } else {
    stop("Test Failed: Negative values detected in 'count'.")
  }
  
  # 6. Test if all station names are within the specified list of valid station names
  ut_stations <- c("Madison Ave / Bloor St W", "Bloor St W / Huron St", "St. George St / Bloor St W", 
                   "Sussex Ave / St George St", "Spadina Ave / Sussex Ave", "Spadina Ave / Harbord St - SMART",
                   "St. George St / Hoskin Ave", "Spadina Ave / Willcocks St", "St. George St / Willcocks St",
                   "Willcocks St / St. George St", "Queen's Park / Bloor St W", "Queen's Park Cres W / Hoskin Ave",
                   "Wellesley St W / Queen's Park Cres", "Queen's Park Cres E / Grosvenor St - SMART", 
                   "Bay St / Bloor St W (East Side)", "Bay St / Bloor St W (West Side)", "Bay St / Charles St W - SMART",
                   "St. Joseph St / Bay St - SMART", "Bay St / St. Joseph St", "Bay St / Wellesley St W", 
                   "Ursula Franklin St / Huron St - SMART", "Ursula Franklin St / St. George St - SMART", "Galbraith Rd / King's College Rd",
                   "College St / Huron St", "College St / Henry St ", "Queens Park Cres / College St ", "University Ave / College St (East)")
  
  if (all(df$station_name %in% ut_stations)) {
    message("Test Passed: All station names are within the specified list.")
  } else {
    stop("Test Failed: Some station names are not within the specified list.")
  }
  
  # 7. Test if 'time' has hours falling into 4-hour intervals
  time_hours <- as.numeric(format(df$time, "%H"))
  valid_intervals <- all(time_hours %% 4 == 0 | time_hours %% 4 == 1 | time_hours %% 4 == 2 | time_hours %% 4 == 3)
  
  if (valid_intervals) {
    message("Test Passed: All time values fall into 4-hour intervals.")
  } else {
    stop("Test Failed: Some time values do not fall into 4-hour intervals.")
  }
  
  # 8. Test if there are any rows with the same 'time' and 'station_name'
  duplicate_rows <- df[duplicated(df[, c("time", "station_name")]), ]
  
  if (nrow(duplicate_rows) == 0) {
    message("Test Passed: No duplicate rows with the same 'time' and 'station_name' found.")
  } else {
    stop("Test Failed: Duplicate rows with the same 'time' and 'station_name' detected.")
  }
}


for (file_path in file_paths) {
  message("\nTesting file: ", file_path)
  tryCatch({
    df <- read_parquet(file_path) 
    df$time <- as.POSIXct(df$time, format="%Y-%m-%d %H:%M:%S") 
    test_dataset(df) 
  }, error = function(e) {
    message("Error in file ", file_path, ": ", e$message)
  })
}
