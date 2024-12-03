#### Preamble ####
# Purpose: exploratory data analysis.
# Author: Haowei Fan
# Date: November 28, 2024
# Contact: haowei.fan@mail.utoronto.ca
# License: Open Government License - Toronto
# Pre-requisites: Scripts with prefixes 00-04 must have been run.
# Any other information needed? Make sure you are in the `Bikeshare-Forecast.Rproj`.
# The code was completed under the guidance of ChatGPT-4, and the detailed interaction process can be found in the path：`other/llm_usage/`


#### Workspace setup ####
set.seed(912)
# List of necessary packages
packages <- c("arrow", "lubridate", "tidyverse", "ggplot2", "reshape2")
# Install missing packages
missing_packages <- packages[!(packages %in% installed.packages()[, "Package"])]
install.packages(missing_packages)
# Load packages
lapply(packages, library, character.only = TRUE)

#### Read data ####
data <- read_parquet("data/02-analysis_data/start.parquet")

#### EDA ####
# Convert 'time' column to datetime format
data$time <- as.POSIXct(data$time, format = "%Y-%m-%d %H:%M:%S")

# Add time-related columns
data$year <- year(data$time)
data$month <- month(data$time)
data$day <- day(data$time)
data$hour <- hour(data$time)
data$weekday <- wday(data$time, label = TRUE)

# 1. Summary Statistics
summary(data)

# 2. Missing Values
colSums(is.na(data))

# 3. Distribution of Count
ggplot(data, aes(x = count)) +
  geom_histogram(bins = 30, fill = "blue", alpha = 0.7) +
  labs(title = "Distribution of Count", x = "Count", y = "Frequency")

# 4. Count by Year
data %>%
  group_by(year) %>%
  summarize(total_count = sum(count), .groups = "drop") %>%
  ggplot(aes(x = factor(year), y = total_count)) +
  geom_bar(stat = "identity", fill = "green") +
  labs(title = "Total Count by Year", x = "Year", y = "Total Count")

# 5. Count by Month
data %>%
  group_by(month) %>%
  summarize(total_count = sum(count), .groups = "drop") %>%
  ggplot(aes(x = factor(month), y = total_count)) +
  geom_bar(stat = "identity", fill = "orange") +
  labs(title = "Total Count by Month", x = "Month", y = "Total Count")

# 6. Count by Weekday
data %>%
  group_by(weekday) %>%
  summarize(total_count = sum(count), .groups = "drop") %>%
  ggplot(aes(x = weekday, y = total_count)) +
  geom_bar(stat = "identity", fill = "purple") +
  labs(title = "Total Count by Weekday", x = "Weekday", y = "Total Count")

# 7. Count by Hour
data %>%
  group_by(hour) %>%
  summarize(total_count = sum(count), .groups = "drop") %>%
  ggplot(aes(x = factor(hour), y = total_count)) +
  geom_bar(stat = "identity", fill = "red") +
  labs(title = "Total Count by Hour", x = "Hour", y = "Total Count")

# 8. Count over Time
data %>%
  group_by(month_year = floor_date(time, "month")) %>%
  summarize(total_count = sum(count), .groups = "drop") %>%
  ggplot(aes(x = month_year, y = total_count)) +
  geom_line(color = "blue") +
  labs(title = "Monthly Total Count Over Time", x = "Time", y = "Total Count")

# 9. Top 10 Stations by Total Count
data %>%
  group_by(station_name) %>%
  summarize(total_count = sum(count), .groups = "drop") %>%
  arrange(desc(total_count)) %>%
  head(10) %>%
  ggplot(aes(x = reorder(station_name, -total_count), y = total_count)) +
  geom_bar(stat = "identity", fill = "cyan") +
  coord_flip() +
  labs(title = "Top 10 Stations by Total Count", x = "Station Name", y = "Total Count")

# 10. Correlation Heatmap
correlation_matrix <- data %>%
  select(count, year, month, day, hour) %>%
  cor()
melted_correlation <- melt(correlation_matrix)
ggplot(melted_correlation, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "red", high = "blue", mid = "white", midpoint = 0, limit = c(-1, 1)) +
  labs(title = "Correlation Heatmap", x = "", y = "")

# 11. Boxplot of Count by Weekday
ggplot(data, aes(x = weekday, y = count)) +
  geom_boxplot() +
  labs(title = "Boxplot of Count by Weekday", x = "Weekday", y = "Count")

# 12. Distribution of Total Count per Station
data %>%
  group_by(station_name) %>%
  summarize(total_count = sum(count), .groups = "drop") %>%
  ggplot(aes(x = total_count)) +
  geom_histogram(bins = 30, fill = "blue", alpha = 0.7) +
  labs(title = "Distribution of Total Count per Station", x = "Total Count", y = "Frequency")

# 13. Average Hourly Count per Station
data %>%
  group_by(station_name, hour) %>%
  summarize(avg_count = mean(count), .groups = "drop") %>%
  ggplot(aes(x = hour, y = station_name, fill = avg_count)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "red") +
  labs(title = "Average Hourly Count per Station", x = "Hour", y = "Station Name")

# 14. Seasonal Trend: Count by Month
data %>%
  group_by(month) %>%
  summarize(avg_count = mean(count), .groups = "drop") %>%
  ggplot(aes(x = factor(month), y = avg_count)) +
  geom_bar(stat = "identity", fill = "green") +
  labs(title = "Average Monthly Count Across All Years", x = "Month", y = "Average Count")

# 15. Unique Stations Over Time
data %>%
  group_by(month_year = floor_date(time, "month")) %>%
  summarize(unique_stations = n_distinct(station_name), .groups = "drop") %>%
  ggplot(aes(x = month_year, y = unique_stations)) +
  geom_line() +
  labs(title = "Number of Unique Stations Over Time", x = "Time", y = "Unique Stations")

# 16. Count Trends for Top 3 Stations
top_3_stations <- data %>%
  group_by(station_name) %>%
  summarize(total_count = sum(count), .groups = "drop") %>%
  arrange(desc(total_count)) %>%
  slice_head(n = 3) %>%
  pull(station_name)

data %>%
  filter(station_name %in% top_3_stations) %>%
  group_by(station_name, month_year = floor_date(time, "month")) %>%
  summarize(total_count = sum(count), .groups = "drop") %>%
  ggplot(aes(x = month_year, y = total_count, color = station_name)) +
  geom_line() +
  labs(title = "Monthly Count Trends for Top 3 Stations", x = "Time", y = "Total Count")

# 17. Weekly Pattern of Counts
data %>%
  group_by(weekday) %>%
  summarize(avg_count = mean(count), .groups = "drop") %>%
  ggplot(aes(x = weekday, y = avg_count)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(title = "Average Count by Weekday", x = "Weekday", y = "Average Count")

# 18. Heatmap of Average Count by Weekday and Hour
data %>%
  group_by(weekday, hour) %>%
  summarize(avg_count = mean(count), .groups = "drop") %>%
  ggplot(aes(x = hour, y = weekday, fill = avg_count)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "purple") +
  labs(title = "Heatmap of Average Count by Weekday and Hour", x = "Hour", y = "Weekday")

# 19. Outlier Detection in Counts
ggplot(data, aes(x = count)) +
  geom_boxplot(fill = "orange", alpha = 0.7) +
  labs(title = "Boxplot of Counts for Outlier Detection", x = "Count")

# 20. Total Count Comparison for Weekdays vs Weekends
data <- data %>%
  mutate(is_weekend = ifelse(weekday %in% c("Sat", "Sun"), "Weekend", "Weekday"))

data %>%
  group_by(is_weekend) %>%
  summarize(total_count = sum(count), .groups = "drop") %>%
  ggplot(aes(x = is_weekend, y = total_count, fill = is_weekend)) +
  geom_bar(stat = "identity") +
  labs(title = "Total Count: Weekdays vs Weekends", x = "Day Type", y = "Total Count")
