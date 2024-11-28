#### Preamble ####
# Purpose: Clean the raw data into the format of simulated data.
# Author: Haowei Fan
# Date: November 28, 2024
# Contact: haowei.fan@mail.utoronto.ca
# License: Open Government License - Toronto
# Pre-requisites: 02-download_data.R must have been run.
# Any other information needed? Make sure you are in the `Bikeshare-Forecast.Rproj`.




#### Workspace setup ####
set.seed(912)
# List of necessary packages
packages <- c("dplyr", "arrow", "lubridate", "stringr", "readr", "tidyverse")
# Install missing packages
missing_packages <- packages[!(packages %in% installed.packages()[, "Package"])]
install.packages(missing_packages)
# Load packages
lapply(packages, library, character.only = TRUE)

#### Clean data ####
# Set the folder path
folder_path <- "data/01-raw_data"
# Get the list of all CSV files in the folder
csv_path <- list.files(path = folder_path, pattern = "\\.csv$", full.names = TRUE)
csv_list <- lapply(csv_path, read.csv)
# Clean up column names and remove unnecessary columns.
for (i in seq_along(csv_list)) {
  file <- csv_list[[i]]
  if ("ï..Trip.Id" %in% colnames(file)) {
    colnames(file)[colnames(file) == "ï..Trip.Id"] <- "trip_id"
  }
  if ("Start.Time" %in% colnames(file)) {
    colnames(file)[colnames(file) == "Start.Time"] <- "trip_start_time"
  }
  if ("Start.Station.Name" %in% colnames(file)) {
    colnames(file)[colnames(file) == "Start.Station.Name"] <- "from_station_name"
  }
  if ("End.Time" %in% colnames(file)) {
    colnames(file)[colnames(file) == "End.Time"] <- "trip_stop_time"
  }
  if ("End.Station.Name" %in% colnames(file)) {
    colnames(file)[colnames(file) == "End.Station.Name"] <- "to_station_name"
  }
  if ("Trip.Id" %in% colnames(file)) {
    colnames(file)[colnames(file) == "Trip.Id"] <- "trip_id"
  }
  file <- file %>% select(trip_id, trip_start_time, from_station_name, trip_stop_time, to_station_name)
  csv_list[[i]] <- file
}

# Create a new dataframe that combines all files
df_combined <- do.call(rbind, csv_list)

# Standardize the date format
pattern <- "^\\d{2}/\\d{2}/\\d{4} \\d{2}:\\d{2}$"
df_combined$trip_start_time <- gsub("^(\\d{2})/(\\d{2})/(\\d{2}) (\\d{2}):(\\d{2}):\\d{2}$", "\\1/\\2/20\\3 \\4:\\5", df_combined$trip_start_time)
df_combined$trip_start_time <- gsub("^(\\d{1})/(\\d{1,2}/\\d{4} \\d{1,2}:\\d{1,2})$", "0\\1/\\2", df_combined$trip_start_time)
df_combined$trip_start_time <- gsub("^(\\d{2})/(\\d{1})/(\\d{4} \\d{1,2}:\\d{1,2})$", "\\1/0\\2/\\3", df_combined$trip_start_time)
df_combined$trip_start_time <- gsub("^(\\d{2}/\\d{2}/\\d{4} )(\\d{1}):(\\d{1,2})$", "\\10\\2:\\3", df_combined$trip_start_time)
df_combined$trip_start_time <- gsub("^(\\d{2}/\\d{2}/\\d{4} \\d{2}:)(\\d{1})$", "\\10\\2", df_combined$trip_start_time)
matches_start <- grepl(pattern, df_combined$trip_start_time)
df_combined <- df_combined[matches_start, ]
df_combined$trip_stop_time <- gsub("^(\\d{2})/(\\d{2})/(\\d{2}) (\\d{2}):(\\d{2}):\\d{2}$", "\\1/\\2/20\\3 \\4:\\5", df_combined$trip_stop_time)
df_combined$trip_stop_time <- gsub("^(\\d{1})/(\\d{1,2}/\\d{4} \\d{1,2}:\\d{1,2})$", "0\\1/\\2", df_combined$trip_stop_time)
df_combined$trip_stop_time <- gsub("^(\\d{2})/(\\d{1})/(\\d{4} \\d{1,2}:\\d{1,2})$", "\\1/0\\2/\\3", df_combined$trip_stop_time)
df_combined$trip_stop_time <- gsub("^(\\d{2}/\\d{2}/\\d{4} )(\\d{1}):(\\d{1,2})$", "\\10\\2:\\3", df_combined$trip_stop_time)
df_combined$trip_stop_time <- gsub("^(\\d{2}/\\d{2}/\\d{4} \\d{2}:)(\\d{1})$", "\\10\\2", df_combined$trip_stop_time)
matches_stop <- grepl(pattern, df_combined$trip_stop_time)
df_combined <- df_combined[matches_stop, ]
df_combined <- df_combined %>%
  mutate(trip_start_time = ifelse(trip_id >= 712382 & trip_id <= 1253913,
                                  format(dmy_hm(trip_start_time), "%m/%d/%Y %H:%M"),
                                  trip_start_time))
df_combined <- df_combined %>%
  mutate(trip_stop_time = ifelse(trip_id >= 712382 & trip_id <= 1253913,
                                 format(dmy_hm(trip_stop_time), "%m/%d/%Y %H:%M"),
                                 trip_stop_time))

# Drop NULLs
df_combined <- subset(df_combined, from_station_name != "NULL")
df_combined <- subset(df_combined, to_station_name != "NULL")

# Find target samples
unique_station <- data_frame(stations = unique(df_combined$from_station_name))
ut_stations <- c("Madison Ave / Bloor St W", "Bloor St W / Huron St", "St. George St / Bloor St W", 
                 "Sussex Ave / St George St", "Spadina Ave / Sussex Ave", "Spadina Ave / Harbord St - SMART",
                 "St. George St / Hoskin Ave", "Spadina Ave / Willcocks St", "St. George St / Willcocks St",
                 "Willcocks St / St. George St", "Queen's Park / Bloor St W", "Queen's Park Cres W / Hoskin Ave",
                 "Wellesley St W / Queen's Park Cres", "Queen's Park Cres E / Grosvenor St - SMART", 
                 "Bay St / Bloor St W (East Side)", "Bay St / Bloor St W (West Side)", "Bay St / Charles St W - SMART",
                 "St. Joseph St / Bay St - SMART", "Bay St / St. Joseph St", "Bay St / Wellesley St W", 
                 "Ursula Franklin St / Huron St - SMART", "Ursula Franklin St / St. George St - SMART", "Galbraith Rd / King's College Rd",
                 "College St / Huron St", "College St / Henry St ", "Queens Park Cres / College St ", "University Ave / College St (East)")
df_combined <- df_combined[df_combined$from_station_name %in% ut_stations, ]
df_combined <- df_combined[df_combined$to_station_name %in% ut_stations, ]

# Process from-data and to-data separately.
df_combined_start <- df_combined %>% select(trip_start_time, from_station_name)
df_combined_stop <- df_combined %>% select(trip_stop_time, to_station_name)
df_combined_start$trip_start_time <- mdy_hm(df_combined_start$trip_start_time)
df_combined_stop$trip_stop_time <- mdy_hm(df_combined_stop$trip_stop_time)

# Create a new column for the time interval (every 4 hours)
df_combined_start <- df_combined_start %>%
  mutate(interval = floor_date(trip_start_time, "4 hours"))
df_combined_stop <- df_combined_stop %>%
  mutate(interval = floor_date(trip_stop_time, "4 hours"))
df_combined_start <- df_combined_start %>% select(-trip_start_time)
df_combined_stop <- df_combined_stop %>% select(-trip_stop_time)

# Group by from(to)_station_name and the new interval, then count the occurrences
result_start <- df_combined_start %>%
  group_by(from_station_name, interval) %>%
  summarise(count = n())%>%
  ungroup()
result_stop <- df_combined_stop %>%
  group_by(to_station_name, interval) %>%
  summarise(count = n())%>%
  ungroup()

# Make sure date are in the right format
result_start$interval <- format(result_start$interval, format="%Y-%m-%d %H:%M:%S")
result_stop$interval <- format(result_stop$interval, format="%Y-%m-%d %H:%M:%S")

# Clean column names
colnames(result_start)[colnames(result_start) == "interval"] <- "time"
colnames(result_stop)[colnames(result_stop) == "interval"] <- "time"
colnames(result_start)[colnames(result_start) == "from_station_name"] <- "station_name"
colnames(result_stop)[colnames(result_stop) == "to_station_name"] <- "station_name"
result_start <- result_start %>% janitor::clean_names()
result_stop <- result_stop %>% janitor::clean_names()

#### Save data ####
write_parquet(result_start, "data/02-analysis_data/start.parquet")
write_parquet(result_stop, "data/02-analysis_data/stop.parquet")

