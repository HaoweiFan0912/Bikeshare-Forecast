#### Preamble ####
# Purpose: Simulate the usage data for each bike-sharing station within the University of Toronto's St. George campus, including the station name, 4-hour time intervals, and the total usage within each time interval.
# Author: Haowei Fan
# Date: November 28, 2024
# Contact: haowei.fan@mail.utoronto.ca
# License: Open Government License - Toronto
# Pre-requisites: None
# Any other information needed? Make sure you are in the `Bikeshare-Forecast.Rproj`.


#### Workspace setup ####
set.seed(912)
# List of necessary packages
packages <- c("tidyverse", "dplyr", "arrow")
# Install missing packages
missing_packages <- packages[!(packages %in% installed.packages()[, "Package"])]
install.packages(missing_packages)
# Load packages
lapply(packages, library, character.only = TRUE)


#### Simulate data ####
ut_stations <- c("Madison Ave / Bloor St W", "Bloor St W / Huron St", "St. George St / Bloor St W", 
                 "Sussex Ave / St George St", "Spadina Ave / Sussex Ave", "Spadina Ave / Harbord St - SMART",
                 "St. George St / Hoskin Ave", "Spadina Ave / Willcocks St", "St. George St / Willcocks St",
                 "Willcocks St / St. George St", "Queen's Park / Bloor St W", "Queen's Park Cres W / Hoskin Ave",
                 "Wellesley St W / Queen's Park Cres", "Queen's Park Cres E / Grosvenor St - SMART", 
                 "Bay St / Bloor St W (East Side)", "Bay St / Bloor St W (West Side)", "Bay St / Charles St W - SMART",
                 "St. Joseph St / Bay St - SMART", "Bay St / St. Joseph St", "Bay St / Wellesley St W", 
                 "Ursula Franklin St / Huron St - SMART", "Ursula Franklin St / St. George St - SMART", "Galbraith Rd / King's College Rd",
                 "College St / Huron St", "College St / Henry St ", "Queens Park Cres / College St ", "University Ave / College St (East)")
all_intervals <- seq(from = as.POSIXct("2017-01-01 00:00:00"), 
                     to = as.POSIXct("2024-09-30 24:00:00"), 
                     by = "4 hours")
all_intervals <- all_intervals - ifelse(as.numeric(format(all_intervals, "%H")) %% 2 == 1, 3600, 0)

all_combinations_start <- expand.grid(from_station_name = ut_stations, interval = all_intervals)
all_combinations_start$interval <- format(all_combinations_start$interval, format="%Y-%m-%d %H:%M:%S")
all_combinations_start <- all_combinations_start %>%
  mutate(count = NA)


all_combinations_start$interval <- ymd_hms(all_combinations_start$interval)
all_combinations_start$year <- year(all_combinations_start$interval)
all_combinations_start$month <- month(all_combinations_start$interval)
all_combinations_start$season <- case_when(
  all_combinations_start$month %in% c(12, 1, 2) ~ "Winter",
  all_combinations_start$month %in% c(3, 4, 5) ~ "Spring",
  all_combinations_start$month %in% c(6, 7, 8) ~ "Summer",
  all_combinations_start$month %in% c(9, 10, 11) ~ "Full"
)

all_combinations_start <- all_combinations_start %>%
  mutate(count = ifelse(year == 2017 & season == "Winter", rpois(sum(year == 2017 & season == "Winter"), lambda = 2), count))
all_combinations_start <- all_combinations_start %>%
  mutate(count = ifelse(year == 2017 & season == "Spring", rpois(sum(year == 2017 & season == "Spring"), lambda = 3), count))
all_combinations_start <- all_combinations_start %>%
  mutate(count = ifelse(year == 2017 & season == "Summer", rpois(sum(year == 2017 & season == "Summer"), lambda = 4), count))
all_combinations_start <- all_combinations_start %>%
  mutate(count = ifelse(year == 2017 & season == "Full", rpois(sum(year == 2017 & season == "Full"), lambda = 3), count))

all_combinations_start <- all_combinations_start %>%
  mutate(count = ifelse(year == 2018 & season == "Winter", rpois(sum(year == 2018 & season == "Winter"), lambda = 2), count))
all_combinations_start <- all_combinations_start %>%
  mutate(count = ifelse(year == 2018 & season == "Spring", rpois(sum(year == 2018 & season == "Spring"), lambda = 4), count))
all_combinations_start <- all_combinations_start %>%
  mutate(count = ifelse(year == 2018 & season == "Summer", rpois(sum(year == 2018 & season == "Summer"), lambda = 6), count))
all_combinations_start <- all_combinations_start %>%
  mutate(count = ifelse(year == 2018 & season == "Full", rpois(sum(year == 2018 & season == "Full"), lambda = 4), count))

all_combinations_start <- all_combinations_start %>%
  mutate(count = ifelse(year == 2019 & season == "Winter", rpois(sum(year == 2019 & season == "Winter"), lambda = 3), count))
all_combinations_start <- all_combinations_start %>%
  mutate(count = ifelse(year == 2019 & season == "Spring", rpois(sum(year == 2019 & season == "Spring"), lambda = 4), count))
all_combinations_start <- all_combinations_start %>%
  mutate(count = ifelse(year == 2019 & season == "Summer", rpois(sum(year == 2019 & season == "Summer"), lambda = 8), count))
all_combinations_start <- all_combinations_start %>%
  mutate(count = ifelse(year == 2019 & season == "Full", rpois(sum(year == 2019 & season == "Full"), lambda = 6), count))

all_combinations_start <- all_combinations_start %>%
  mutate(count = ifelse(year == 2020 & season == "Winter", rpois(sum(year == 2020 & season == "Winter"), lambda = 3), count))
all_combinations_start <- all_combinations_start %>%
  mutate(count = ifelse(year == 2020 & season == "Spring", rpois(sum(year == 2020 & season == "Spring"), lambda = 5), count))
all_combinations_start <- all_combinations_start %>%
  mutate(count = ifelse(year == 2020 & season == "Summer", rpois(sum(year == 2020 & season == "Summer"), lambda = 10), count))
all_combinations_start <- all_combinations_start %>%
  mutate(count = ifelse(year == 2020 & season == "Full", rpois(sum(year == 2020 & season == "Full"), lambda = 7), count))

all_combinations_start <- all_combinations_start %>%
  mutate(count = ifelse(year == 2021 & season == "Winter", rpois(sum(year == 2021 & season == "Winter"), lambda = 4), count))
all_combinations_start <- all_combinations_start %>%
  mutate(count = ifelse(year == 2021 & season == "Spring", rpois(sum(year == 2021 & season == "Spring"), lambda = 7), count))
all_combinations_start <- all_combinations_start %>%
  mutate(count = ifelse(year == 2021 & season == "Summer", rpois(sum(year == 2021 & season == "Summer"), lambda = 12), count))
all_combinations_start <- all_combinations_start %>%
  mutate(count = ifelse(year == 2021 & season == "Full", rpois(sum(year == 2021 & season == "Full"), lambda = 10), count))

all_combinations_start <- all_combinations_start %>%
  mutate(count = ifelse(year == 2022 & season == "Winter", rpois(sum(year == 2022 & season == "Winter"), lambda = 4), count))
all_combinations_start <- all_combinations_start %>%
  mutate(count = ifelse(year == 2022 & season == "Spring", rpois(sum(year == 2022 & season == "Spring"), lambda = 7), count))
all_combinations_start <- all_combinations_start %>%
  mutate(count = ifelse(year == 2022 & season == "Summer", rpois(sum(year == 2022 & season == "Summer"), lambda = 14), count))
all_combinations_start <- all_combinations_start %>%
  mutate(count = ifelse(year == 2022 & season == "Full", rpois(sum(year == 2022 & season == "Full"), lambda = 11), count))

all_combinations_start <- all_combinations_start %>%
  mutate(count = ifelse(year == 2023 & season == "Winter", rpois(sum(year == 2023 & season == "Winter"), lambda = 5), count))
all_combinations_start <- all_combinations_start %>%
  mutate(count = ifelse(year == 2023 & season == "Spring", rpois(sum(year == 2023 & season == "Spring"), lambda = 8), count))
all_combinations_start <- all_combinations_start %>%
  mutate(count = ifelse(year == 2023 & season == "Summer", rpois(sum(year == 2023 & season == "Summer"), lambda = 16), count))
all_combinations_start <- all_combinations_start %>%
  mutate(count = ifelse(year == 2023 & season == "Full", rpois(sum(year == 2023 & season == "Full"), lambda = 12), count))

all_combinations_start <- all_combinations_start %>%
  mutate(count = ifelse(year == 2024 & season == "Winter", rpois(sum(year == 2024 & season == "Winter"), lambda = 5), count))
all_combinations_start <- all_combinations_start %>%
  mutate(count = ifelse(year == 2024 & season == "Spring", rpois(sum(year == 2024 & season == "Spring"), lambda = 9), count))
all_combinations_start <- all_combinations_start %>%
  mutate(count = ifelse(year == 2024 & season == "Summer", rpois(sum(year == 2024 & season == "Summer"), lambda = 18), count))
all_combinations_start <- all_combinations_start %>%
  mutate(count = ifelse(year == 2024 & season == "Full", rpois(sum(year == 2024 & season == "Full"), lambda = 13), count))

all_combinations_start <- all_combinations_start %>% select(from_station_name, interval, count)
colnames(all_combinations_start)[colnames(all_combinations_start) == "interval"] <- "time"
colnames(all_combinations_start)[colnames(all_combinations_start) == "from_station_name"] <- "station_name"
all_combinations_stop <- all_combinations_start %>%
  mutate(count = floor(count * 0.4))

#### Save data ####
write_parquet(all_combinations_start, "data/00-simulated_data/start_simulation.parquet")
write_parquet(all_combinations_stop, "data/00-simulated_data/stop_simulation.parquet")
