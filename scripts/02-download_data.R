#### Preamble ####
# Purpose: Downloads and saves the data from https://open.toronto.ca/dataset/bike-share-toronto-ridership-data/
# Author: Haowei Fan
# Date: November 28, 2024
# Contact: haowei.fan@mail.utoronto.ca
# License: Open Government License - Toronto
# Pre-requisites: Be able to access this website: https://open.toronto.ca/dataset/bike-share-toronto-ridership-data/.
# Any other information needed? Make sure you are in the `Bikeshare-Forecast.Rproj`.


#### Workspace setup ####
set.seed(912)
# List of necessary packages
packages <- c("opendatatoronto", "dplyr", "stringr", "readr")
# Install missing packages
missing_packages <- packages[!(packages %in% installed.packages()[, "Package"])]
install.packages(missing_packages)
# Load packages
lapply(packages, library, character.only = TRUE)

#### Download data ####
resources_url <- "https://open.toronto.ca/dataset/bike-share-toronto-ridership-data/"
data_2017 <- list_package_resources(resources_url) %>%
  filter(name == "bikeshare-ridership-2017") %>%
  get_resource()
data_2018 <- list_package_resources(resources_url) %>%
  filter(name == "bikeshare-ridership-2018") %>%
  get_resource()
data_2019 <- list_package_resources(resources_url) %>%
  filter(name == "bikeshare-ridership-2019") %>%
  get_resource()
data_2020 <- list_package_resources(resources_url) %>%
  filter(name == "bikeshare-ridership-2020") %>%
  get_resource()
data_2021 <- list_package_resources(resources_url) %>%
  filter(name == "bikeshare-ridership-2021") %>%
  get_resource()
data_2022 <- list_package_resources(resources_url) %>%
  filter(name == "bikeshare-ridership-2022") %>%
  get_resource()
data_2023 <- list_package_resources(resources_url) %>%
  filter(name == "bikeshare-ridership-2023") %>%
  get_resource()
data_2024 <- list_package_resources(resources_url) %>%
  filter(name == "bikeshare-ridership-2024") %>%
  get_resource()

# Delete all non-data-frame format files
data_2022 <- data_2022[!names(data_2022) %in% c("Bike share ridership 2022-11.zip")]
data_2017 <- data_2017[-1]
data_2018 <- data_2018[-1]

#### Save data ####
# Handling irregular file formats
download.file(
  "https://ckan0.cf.opendata.inter.prod-toronto.ca/dataset/7e876c24-177c-4605-9cef-e50dd74c617f/resource/db10a7b1-2702-481c-b7f0-0c67070104bb/download/bikeshare-ridership-2022.zip",
  "data/01-raw_data/zip_2022.zip"
)
unzip("data/01-raw_data/zip_2022.zip", exdir = "data/01-raw_data/")
unzip("data/01-raw_data/bikeshare-ridership-2022/Bike share ridership 2022-11.zip", exdir = "data/01-raw_data/")
file.remove("data/01-raw_data/zip_2022.zip")
unlink("data/01-raw_data/bikeshare-ridership-2022", recursive = TRUE)
# Save files in a right format
for (i in seq_along(data_2017)) {
  filename <- names(data_2017)[i]
  sampled_df <- data_2017[[i]]
  write.csv(sampled_df, file = paste0("data/01-raw_data/", filename), row.names = FALSE)
}
for (i in seq_along(data_2018)) {
  filename <- names(data_2018)[i]
  sampled_df <- data_2018[[i]]
  write.csv(sampled_df, file = paste0("data/01-raw_data/", filename), row.names = FALSE)
}
for (i in seq_along(data_2019)) {
  filename <- names(data_2019)[i]
  sampled_df <- data_2019[[i]]
  write.csv(sampled_df, file = paste0("data/01-raw_data/", filename), row.names = FALSE)
}
for (i in seq_along(data_2020)) {
  filename <- names(data_2020)[i]
  sampled_df <- data_2020[[i]]
  write.csv(sampled_df, file = paste0("data/01-raw_data/", filename), row.names = FALSE)
}
for (i in seq_along(data_2021)) {
  filename <- names(data_2021)[i]
  sampled_df <- data_2021[[i]]
  write.csv(sampled_df, file = paste0("data/01-raw_data/", filename), row.names = FALSE)
}
for (i in seq_along(data_2022)) {
  filename <- names(data_2022)[i]
  sampled_df <- data_2022[[i]]
  write.csv(sampled_df, file = paste0("data/01-raw_data/", filename), row.names = FALSE)
}
for (i in seq_along(data_2023)) {
  filename <- names(data_2023)[i]
  sampled_df <- data_2023[[i]]
  write.csv(sampled_df, file = paste0("data/01-raw_data/", filename), row.names = FALSE)
}
for (i in seq_along(data_2024)) {
  filename <- names(data_2024)[i]
  sampled_df <- data_2024[[i]]
  write.csv(sampled_df, file = paste0("data/01-raw_data/", filename), row.names = FALSE)
}

#### Rename files ####
# Set the folder path
folder_path <- "data/01-raw_data"
# General name cleaning

file_names <- list.files(path = folder_path, pattern = "*.csv", full.names = TRUE) # This segment of code was completed with the assistance of ChatGPT-4.
# Define the patterns to be removed from file names
patterns_to_remove <- c("Bike", "Share", "Ridership", "Toronto", "bike", "share", "ridership", "toronto", " ", "\\(", "\\)")
# Iterate through all files and modify the file names

for (file in file_names) {
  old_name <- basename(file)
  new_name <- old_name
  for (pattern in patterns_to_remove) {
    new_name <- str_remove_all(new_name, pattern)
  }
  new_file_path <- file.path(folder_path, new_name)
  file.rename(file, new_file_path)
} # This segment of code was completed with the assistance of ChatGPT-4.
# Rename the 2017 data files
files_2017 <- list.files(path = folder_path, pattern = "2017", full.names = TRUE)
# Loop through each file and rename it

for (file in files_2017) {
  file_name <- basename(file)
  new_name <- gsub("2017Q(\\d+)", "2017-Q\\1", file_name) # Change '2017Qx' to '2017-Qx' format
  new_path <- file.path(folder_path, new_name) # Generate the new file path
  if (file != new_path) { # Ensure the new and old paths are different
    file.rename(file, new_path) # Rename the file
  }
} # This segment of code was completed with the assistance of ChatGPT-4.
# Rename the 2018 data files

files_2018 <- list.files(path = folder_path, pattern = "2018", full.names = TRUE) # This segment of code was completed with the assistance of ChatGPT-4.
# Loop through each file and rename it
for (file in files_2018) {
  file_name <- basename(file)
  new_name <- gsub("_Q(\\d+)2018", "2018-Q\\1", file_name) # Change '_Qx2018' to '2018-Qx' format
  new_path <- file.path(folder_path, new_name) # Generate the new file path
  if (file != new_path) { # Ensure the new and old paths are different
    file.rename(file, new_path) # Rename the file
  }
}

#### Split files larger than 90MB####
# Get the paths of all CSV files in the folder
all_csv_files <- list.files(folder_path, pattern = "*.csv", full.names = TRUE)
# Find files larger than 90MB
large_csv_files <- all_csv_files[file.info(all_csv_files)$size > 90 * 1024 * 1024]
# Define a function to split the file into two parts
split_csv <- function(file_path) {
  # Read the CSV file
  df <- read.csv(file_path, stringsAsFactors = FALSE)
  # Calculate the split point
  split_index <- ceiling(nrow(df) / 2)
  # Split the dataframe
  df1 <- df[1:split_index, ]
  df2 <- df[(split_index + 1):nrow(df), ]
  # Create new file names
  file_base_name <- tools::file_path_sans_ext(basename(file_path))
  new_file_1 <- file.path(dirname(file_path), paste0(file_base_name, "-1.csv"))
  new_file_2 <- file.path(dirname(file_path), paste0(file_base_name, "-2.csv"))
  # Write the two new CSV files
  write.csv(df1, new_file_1, row.names = FALSE)
  write.csv(df2, new_file_2, row.names = FALSE)
  # Delete the original large file
  file.remove(file_path)
}
# Split each file larger than 100MB
lapply(large_csv_files, split_csv)
