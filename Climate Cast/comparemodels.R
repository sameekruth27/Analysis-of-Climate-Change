library(dplyr)
library(forecast)

# Read the data
data <- read.csv("Climate/final_data.csv", header = TRUE)

# Select data for a specific country
iso3_country <- "IND"
selected_data <- data %>%
  filter(iso3 == iso3_country)

# Convert year to Date format
selected_data$year <- as.Date(as.character(selected_data$year), format = "%Y")

# Create a time series object
ts_data <- ts(
  selected_data$air_quality_in_mg_per_m3,
  frequency = 1, start = min(selected_data$year)
)

# Build a linear regression model for year and air quality value
linear_model <- lm(air_quality_in_mg_per_m3 ~ year, data = selected_data)

# ARIMA model
arima_model <- auto.arima(ts_data)
summary(arima_model)

# Exponential smoothing model (ETS)
ets_model <- ets(ts_data)

# Generate forecasts
future_years <- seq(
  max(selected_data$year) + 1, max(selected_data$year) + 5,
  by = "years"
)

# Predict using linear regression
linear_forecast <- predict(linear_model, newdata = data.frame(year = future_years))

# Forecast using ARIMA
forecast_arima <- forecast(arima_model, h = length(future_years))

# Forecast using ETS
forecast_ets <- forecast(ets_model, h = length(future_years))

# Create a comparison dataframe
comparison <- data.frame(
  Year = future_years,
  Linear = linear_forecast,
  ARIMA = forecast_arima$mean,
  ETS = forecast_ets$mean
)

print(comparison)

# Calculate the absolute errors for each model
comparison$Linear_Error <- abs(comparison$Linear - selected_data$air_quality_in_mg_per_m3)
comparison$ARIMA_Error <- abs(comparison$ARIMA - selected_data$air_quality_in_mg_per_m3)
comparison$ETS_Error <- abs(comparison$ETS - selected_data$air_quality_in_mg_per_m3)

# Print the mean absolute error for each year
mean_abs_error <- comparison %>%
  group_by(Year) %>%
  summarize(
    Linear_MAE = mean(Linear_Error),
    ARIMA_MAE = mean(ARIMA_Error),
    ETS_MAE = mean(ETS_Error)
  )

print(mean_abs_error)
