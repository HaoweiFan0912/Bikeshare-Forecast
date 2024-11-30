#### Preamble ####
# Purpose: Fit the model to the cleaned data.
# Author: Haowei Fan
# Date: November 28, 2024
# Contact: haowei.fan@mail.utoronto.ca
# License: Open Government License - Toronto
# Pre-requisites: 03-clean_data.R must have been run.
# Any other information needed? Make sure you are in the `Bikeshare-Forecast.Rproj`.

#### Workspace setup ####
set.seed(912)
# List of necessary packages
packages <- c("arrow", "lubridate", "dplyr", "brms", "rstanarm","tidyverse")
# Install missing packages
missing_packages <- packages[!(packages %in% installed.packages()[, "Package"])]
install.packages(missing_packages)
# Load packages
lapply(packages, library, character.only = TRUE)

#### Read data ####
df <- read_parquet(here::here("data/02-analysis_data/start.parquet"))
df <- df %>%
  mutate(time = as.POSIXct(time, format = "%Y-%m-%d %H:%M:%S"))
ut_stations <- c("Madison Ave / Bloor St W", "Bloor St W / Huron St", "St. George St / Bloor St W", 
                 "Sussex Ave / St George St", "Spadina Ave / Sussex Ave", "Spadina Ave / Harbord St - SMART",
                 "St. George St / Hoskin Ave", "Spadina Ave / Willcocks St", "St. George St / Willcocks St",
                 "Willcocks St / St. George St", "Queen's Park / Bloor St W", "Queen's Park Cres W / Hoskin Ave",
                 "Wellesley St W / Queen's Park Cres", "Queen's Park Cres E / Grosvenor St - SMART", 
                 "Bay St / Bloor St W (East Side)", "Bay St / Bloor St W (West Side)", "Bay St / Charles St W - SMART",
                 "St. Joseph St / Bay St - SMART", "Bay St / St. Joseph St", "Bay St / Wellesley St W", 
                 "Ursula Franklin St / Huron St - SMART", "Ursula Franklin St / St. George St - SMART", "Galbraith Rd / King's College Rd",
                 "College St / Huron St", "College St / Henry St ", "Queens Park Cres / College St ", "University Ave / College St (East)")

#### Loop through each station and create models ####
for (station in ut_stations) {
  station_data <- subset(df, station_name == station)
  station_data <- station_data %>% select(-station_name)
  station_data$time <- as.POSIXct(station_data$time, format = "%Y-%m-%d %H:%M:%S")
  
  # Remove outliers
  Q1 <- quantile(station_data$count, 0.25)
  Q3 <- quantile(station_data$count, 0.75)
  IQR <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  station_data <- subset(station_data, count >= lower_bound & count <= upper_bound)
  train <- station_data %>%
    mutate(
      hour = hour(time),
      day = day(time),
      month = month(time),
      year = year(time)
    )
  
  # Fit Bayesian regression model
  # Remove 'year' if it is a constant
  if (length(unique(train$year)) == 1) {
    model_formula <- count ~ hour + day + month
  } else {
    model_formula <- count ~ hour + day + month + year
  }
  
  # Fit Bayesian regression model
  model <- stan_glm(
    model_formula,
    data = train,
    family = poisson(link = "log"),
    prior = normal(0, 2),
    prior_intercept = normal(0, 5),
    chains = 4, iter = 2000
  )
  
  # Save model
  saveRDS(model, file = paste0("models/", gsub("[ /]", "_", station), ".rds"))
}
