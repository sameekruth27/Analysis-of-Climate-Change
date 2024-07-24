# Load necessary libraries
library(dplyr)
library(forecast)

# Read the dataset
data <- read.csv("merged.csv")  # Replace 'your_dataset.csv' with the path to your dataset

# Select air quality values for a specific country by ISO3 value
iso3_country <- "IND"  # Replace 'USA' with the ISO3 value of the desired country
selected_data <- data %>%
  filter(iso3 == iso3_country)

# Check the structure of selected data
str(selected_data)

# Convert the 'year' column to Date class
selected_data$year <- as.Date(as.character(selected_data$year), format = "%Y")

# Create a time series object
ts_data <- ts(selected_data$air_quality_in_mg_per_m3, frequency = 1, start = min(selected_data$year))

# Check the structure of the time series object
str(ts_data)

# Fit an ARIMA model
arima_model <- auto.arima(ts_data)

# Summary of the ARIMA model
summary(arima_model)

# Forecast air quality value for future years
future_years <- seq(max(selected_data$year) + 1, max(selected_data$year) + 5, by = "years")
forecast_values <- forecast(arima_model, h = length(future_years))

# Print the forecasted air quality values
print(forecast_values)
