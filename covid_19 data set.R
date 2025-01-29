# Load required libraries
library(tidyverse)
library(lubridate)
library(forecast)
library(ggplot2)
library(cluster)
library(dplyr)

# Load data set (modify file path if needed)
covid_data <- read.csv(choose.files())
covid_data
# Convert 'ObservationDate' to Date format
covid_data$ObservationDate <- as.Date(covid_data$ObservationDate, 
                                      format="%m/%d/%Y")

# Check for missing values
missing_values <- colSums(is.na(covid_data))
missing_values

# Filter data for global trend analysis
global_data <- covid_data %>%
  group_by(ObservationDate) %>%
  summarise(
    Confirmed = sum(Confirmed, na.rm = TRUE),
    Deaths = sum(Deaths, na.rm = TRUE),
    Recovered = sum(Recovered, na.rm = TRUE)
  )
global_data

# Calculate Case Fatality Rate (CFR) and Recovery Rate
global_data <- global_data %>%
  mutate(CFR = (Deaths / Confirmed) * 100,
         RecoveryRate = (Recovered / Confirmed) * 100)

global_data

# Time Series Visualization
ggplot(global_data, aes(x = ObservationDate)) +
  geom_line(aes(y = Confirmed, color = "Confirmed Cases")) +
  geom_line(aes(y = Deaths, color = "Deaths")) +
  geom_line(aes(y = Recovered, color = "Recovered Cases")) +
  labs(title = "COVID-19 Global Trend",
       x = "Date",
       y = "Cases Count") +
  theme_minimal()

# ARIMA Time Series Forecasting (For Confirmed Cases)
confirmed_ts <- ts(global_data$Confirmed, frequency = 7)  # Weekly trend
confirmed_ts
arima_model <- auto.arima(confirmed_ts)
arima_model
forecast_values <- forecast(arima_model, h=30)
forecast_values

# Plot forecast
autoplot(forecast_values) +
  labs(title = "COVID-19 Confirmed Cases Forecast",
       x = "Time",
       y = "Predicted Cases")

# Clustering: Identify High-risk Countries
country_data <- covid_data %>%
  group_by(Country.Region) %>%
  summarise(
    TotalConfirmed = sum(Confirmed, na.rm = TRUE),
    TotalDeaths = sum(Deaths, na.rm = TRUE),
    TotalRecovered = sum(Recovered, na.rm = TRUE)
  )
country_data

# Perform K-Means Clustering
set.seed(123)
country_cluster <- kmeans(country_data[,2:4], centers = 3)# 3 Clusters
country_cluster
country_data$Cluster <- as.factor(country_cluster$cluster)
country_data$Cluster
# Visualize clusters
ggplot(country_data, aes(x = TotalConfirmed, y = TotalDeaths, color = Cluster)) +
  geom_point(size=3) +
  scale_x_log10() + scale_y_log10() +
  labs(title = "Clustering of COVID-19 Affected Countries",
       x = "Total Confirmed Cases (Log Scale)",
       y = "Total Deaths (Log Scale)") +
  theme_minimal()
