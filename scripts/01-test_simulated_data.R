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
packages <- c("tidyverse", "dplyr", "arrow", "testthat")
# Install missing packages
missing_packages <- packages[!(packages %in% installed.packages()[, "Package"])]
install.packages(missing_packages)
# Load packages
lapply(packages, library, character.only = TRUE)

#### Read data ####
data_1 <- read_parquet(here::here("data/00-simulated_data/start_simulation.parquet"))
data_2 <- read_parquet(here::here("data/00-simulated_data/stop_simulation.parquet"))

#### Test data ####
# 1. Test if data is loaded successfully
test_that("Data is loaded successfully", {
  expect_s3_class(data_1, "tbl_df")
  expect_gt(nrow(data_1), 0)
})

# 2. Test if data_1 is of data.frame type
test_that("Data is of data_1.frame type", {
  expect_true(is.data.frame(data_1))
})

# 3. Test if column names are correct
test_that("Column names are as expected", {
  expected_colnames <- c("station_name", "time", "count")
  expect_equal(colnames(data_1), expected_colnames)
})

# 4. Test if the time column has no missing values
test_that("Time column has no missing values", {
  expect_true(all(!is.na(data_1$time)))
})

# 5. Test if the count column contains non-negative values
test_that("Count column contains non-negative values", {
  expect_true(all(data_1$count >= 0))
})

# 6. Test if there are no duplicate rows in the dataset
test_that("No duplicate rows in the dataset", {
  expect_equal(nrow(data_1), nrow(distinct(data_1)))
})

# 7. Test if the count column contains only integer values
test_that("Count column contains only integer values", {
  expect_true(all(data_1$count == as.integer(data_1$count)))
})

# 8. Test if there are no missing values in the dataset
test_that("No missing values in the dataset", {
  expect_true(all(complete.cases(data_1)))
})
# 9. Test if the dataset contains at least one record
test_that("Dataset contains at least one record", {
  expect_gt(nrow(data_1), 0)
})

# 10. Test if the time column does not contain future dates
test_that("Time column does not contain future dates", {
  expect_true(all(as.POSIXct(data_1$time) <= Sys.time()))
})
# 11. Test if the mean of the count column is greater than 0
test_that("Mean of count column is greater than 0", {
  expect_gt(mean(data_1$count), 0)
})

# 12. Test if time values are within the specified range
test_that("Time values are within the specified range", {
  time_start <- as.POSIXct("2017-01-01 00:00:00", format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
  time_end <- as.POSIXct("2024-09-30 24:00:00", format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
  data_1$time <- as.POSIXct(data_1$time, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
  expect_true(all(data_1$time >= time_start & data_1$time <= time_end))
})

# 13. Test if there are no duplicate rows in the dataset
test_that("No duplicate rows in the dataset", {
  expect_equal(nrow(data_1), nrow(unique(data_1)))
})

# 14. Test if all station names are within the specified list of valid station names
test_that("All station names are within the specified list of valid station names", {
  ut_stations <- c(
    "Madison Ave / Bloor St W", "Bloor St W / Huron St", "St. George St / Bloor St W",
    "Sussex Ave / St George St", "Spadina Ave / Sussex Ave", "Spadina Ave / Harbord St - SMART",
    "St. George St / Hoskin Ave", "Spadina Ave / Willcocks St", "St. George St / Willcocks St",
    "Willcocks St / St. George St", "Queen's Park / Bloor St W", "Queen's Park Cres W / Hoskin Ave",
    "Wellesley St W / Queen's Park Cres", "Queen's Park Cres E / Grosvenor St - SMART",
    "Bay St / Bloor St W (East Side)", "Bay St / Bloor St W (West Side)", "Bay St / Charles St W - SMART",
    "St. Joseph St / Bay St - SMART", "Bay St / St. Joseph St", "Bay St / Wellesley St W",
    "Ursula Franklin St / Huron St - SMART", "Ursula Franklin St / St. George St - SMART", "Galbraith Rd / King's College Rd",
    "College St / Huron St", "College St / Henry St ", "Queens Park Cres / College St ", "University Ave / College St (East)"
  )
  expect_true(all(data_1$station_name %in% ut_stations))
})

# 15. Test if 'time' has hours falling into 4-hour intervals
test_that("Time values fall into 4-hour intervals", {
  ti <- as.POSIXct(data_1$time, format = "%Y-%m-%d %H:%M:%S")
  time_hours <- as.numeric(format(ti, "%H"))
  valid_intervals <- all(time_hours %% 4 == 0 | time_hours %% 4 == 1 | time_hours %% 4 == 2 | time_hours %% 4 == 3)
  expect_true(valid_intervals)
})

# 16. Test if there are no rows with the same 'time' and 'station_name'
test_that("No duplicate rows with the same 'time' and 'station_name'", {
  duplicate_rows <- data_1[duplicated(data_1[, c("time", "station_name")]), ]
  expect_equal(nrow(duplicate_rows), 0)
})

# 17. Test if data is loaded successfully
test_that("Data is loaded successfully", {
  expect_s3_class(data_2, "tbl_df")
  expect_gt(nrow(data_2), 0)
})

# 18. Test if data_2 is of data.frame type
test_that("Data is of data_2.frame type", {
  expect_true(is.data.frame(data_2))
})

# 19. Test if column names are correct
test_that("Column names are as expected", {
  expected_colnames <- c("station_name", "time", "count")
  expect_equal(colnames(data_2), expected_colnames)
})

# 20. Test if the time column has no missing values
test_that("Time column has no missing values", {
  expect_true(all(!is.na(data_2$time)))
})

# 21. Test if the count column contains non-negative values
test_that("Count column contains non-negative values", {
  expect_true(all(data_2$count >= 0))
})

# 22. Test if there are no duplicate rows in the dataset
test_that("No duplicate rows in the dataset", {
  expect_equal(nrow(data_2), nrow(distinct(data_2)))
})

# 23. Test if the count column contains only integer values
test_that("Count column contains only integer values", {
  expect_true(all(data_2$count == as.integer(data_2$count)))
})

# 24. Test if there are no missing values in the dataset
test_that("No missing values in the dataset", {
  expect_true(all(complete.cases(data_2)))
})
# 25. Test if the dataset contains at least one record
test_that("Dataset contains at least one record", {
  expect_gt(nrow(data_2), 0)
})

# 26. Test if the time column does not contain future dates
test_that("Time column does not contain future dates", {
  expect_true(all(as.POSIXct(data_2$time) <= Sys.time()))
})
# 27. Test if the mean of the count column is greater than 0
test_that("Mean of count column is greater than 0", {
  expect_gt(mean(data_2$count), 0)
})

# 28. Test if time values are within the specified range
test_that("Time values are within the specified range", {
  time_start <- as.POSIXct("2017-01-01 00:00:00", format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
  time_end <- as.POSIXct("2024-09-30 24:00:00", format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
  data_2$time <- as.POSIXct(data_2$time, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
  expect_true(all(data_2$time >= time_start & data_2$time <= time_end))
})

# 29. Test if there are no duplicate rows in the dataset
test_that("No duplicate rows in the dataset", {
  expect_equal(nrow(data_2), nrow(unique(data_2)))
})

# 30. Test if all station names are within the specified list of valid station names
test_that("All station names are within the specified list of valid station names", {
  ut_stations <- c(
    "Madison Ave / Bloor St W", "Bloor St W / Huron St", "St. George St / Bloor St W",
    "Sussex Ave / St George St", "Spadina Ave / Sussex Ave", "Spadina Ave / Harbord St - SMART",
    "St. George St / Hoskin Ave", "Spadina Ave / Willcocks St", "St. George St / Willcocks St",
    "Willcocks St / St. George St", "Queen's Park / Bloor St W", "Queen's Park Cres W / Hoskin Ave",
    "Wellesley St W / Queen's Park Cres", "Queen's Park Cres E / Grosvenor St - SMART",
    "Bay St / Bloor St W (East Side)", "Bay St / Bloor St W (West Side)", "Bay St / Charles St W - SMART",
    "St. Joseph St / Bay St - SMART", "Bay St / St. Joseph St", "Bay St / Wellesley St W",
    "Ursula Franklin St / Huron St - SMART", "Ursula Franklin St / St. George St - SMART", "Galbraith Rd / King's College Rd",
    "College St / Huron St", "College St / Henry St ", "Queens Park Cres / College St ", "University Ave / College St (East)"
  )
  expect_true(all(data_2$station_name %in% ut_stations))
})

# 31. Test if 'time' has hours falling into 4-hour intervals
test_that("Time values fall into 4-hour intervals", {
  ti <- as.POSIXct(data_2$time, format = "%Y-%m-%d %H:%M:%S")
  time_hours <- as.numeric(format(ti, "%H"))
  valid_intervals <- all(time_hours %% 4 == 0 | time_hours %% 4 == 1 | time_hours %% 4 == 2 | time_hours %% 4 == 3)
  expect_true(valid_intervals)
})

# 32. Test if there are no rows with the same 'time' and 'station_name'
test_that("No duplicate rows with the same 'time' and 'station_name'", {
  duplicate_rows <- data_2[duplicated(data_2[, c("time", "station_name")]), ]
  expect_equal(nrow(duplicate_rows), 0)
})
