#### Preamble ####
# Purpose: Models... [...UPDATE THIS...]
# Author: Rohan Alexander [...UPDATE THIS...]
# Date: 11 February 2023 [...UPDATE THIS...]
# Contact: rohan.alexander@utoronto.ca [...UPDATE THIS...]
# License: MIT
# Pre-requisites: [...UPDATE THIS...]
# Any other information needed? [...UPDATE THIS...]


#### Workspace setup ####
set.seed(912)
# List of necessary packages
packages <- c("arrow", "lubridate", "dplyr", "brms", "rstanarm","tidyverse","forecast","tseries")
# Install missing packages
missing_packages <- packages[!(packages %in% installed.packages()[, "Package"])]
install.packages(missing_packages)
# Load packages
lapply(packages, library, character.only = TRUE)

#### Read data ####
df <- read_parquet(here::here("data/02-analysis_data/start.parquet"))
# 将 time 列转换为日期时间格式
df <- df %>%
  mutate(time = as.POSIXct(time, format = "%Y-%m-%d %H:%M:%S"))



SpadinaHarbord <- subset(df, station_name == "Queen's Park Cres E / Grosvenor St - SMART")
SpadinaHarbord <- SpadinaHarbord %>% select(-station_name)
SpadinaHarbord$time <- as.POSIXct(SpadinaHarbord$time, format = "%Y-%m-%d %H:%M:%S")
train_index <- sample(1:nrow(SpadinaHarbord), 0.3 * nrow(SpadinaHarbord))

train <- SpadinaHarbord[train_index, ]
test <- SpadinaHarbord[-train_index, ]
# Convert 'time' column to date-time format


# Split time into multiple features
train <- train %>%
  mutate(
    hour = hour(time),
    day = day(time),
    month = month(time),
    year = year(time)
  )

# Fit Bayesian regression model
model <- stan_glm(
  count ~ hour + day + month + year,
  data = train,
  family = poisson(link = "log"),  # Assuming count is count data, use Poisson distribution
  prior = normal(0, 2),
  prior_intercept = normal(0, 5),
  chains = 4, iter = 2000
)

# Summarize model
print(summary(model))

# Make prediction
test <- test %>%
  mutate(
    hour = hour(time),
    day = day(time),
    month = month(time),
    year = year(time)
  )
predictions <- predict(model, newdata = drop_na(test))

data_compa <- data.frame(
  actual = test$count,
  predicted = predictions
)

ggplot(data_compa, aes(x = actual, y = exp(predicted)))+
  geom_point(color = "blue") +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  labs(
    title = "真实值 vs 预测值",
    x = "True",
    y = "Predi") 












#### Save model ####
#saveRDS(
#  first_model,
#  file = "models/first_model.rds"
#)


