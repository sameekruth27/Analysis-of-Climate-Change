library(dplyr)
library(forecast)
library(ggplot2)

# Load your data
data <- z_score_normalized_data

# Specify the country code
iso3_country <- "IND"

# Function to make predictions and plot for a given variable
predict_and_plot <- function(variable) {
  # Filter the data for training and testing
  train_data <- data %>%
    filter(iso3 == iso3_country) %>%
    filter(year < 2015)
  
  # Convert year to numeric
  train_data$year <- as.numeric(train_data$year)
  
  # Create time series data
  ts_data <- ts(train_data[[variable]], frequency = 1, start = min(train_data$year))
  
  # Fit models
  linear_model <- lm(train_data[[variable]] ~ year, data = train_data)
  arima_model <- auto.arima(ts_data)
  ets_model <- ets(ts_data)
  
  # Define future years from 2020 to 2050
  future_years <- 2020:2050
  
  # Make predictions using the ensemble model
  forecast_linear <- predict(linear_model, newdata = data.frame(year = future_years))
  forecast_arima <- forecast(arima_model, h = length(future_years))
  forecast_ets <- forecast(ets_model, h = length(future_years))
  ensemble_forecast <- (weights[1] * forecast_linear) +
    (weights[2] * forecast_arima$mean) +
    (weights[3] * forecast_ets$mean)
  
  # Create a data frame with the predicted values and the corresponding years
  ensemble_predictions <- data.frame(
    Year = future_years,
    Ensemble_Predictions = ensemble_forecast
  )
  
  # Save the ensemble predictions to a CSV file
  write.csv(ensemble_predictions, file = paste0("ensemble_predictions_", variable, ".csv"), row.names = FALSE)
  
  # Read the ensemble predictions from the CSV file
  ensemble_predictions <- read.csv(paste0("ensemble_predictions_", variable, ".csv"))
  
  # Filter historical data for the years 1999 to 2020
  historical_data <- data %>%
    filter(iso3 == iso3_country & year >= 1999 & year < 2020)
  
  # Plot historical data and ensemble predictions
  ggplot() +
    geom_line(data = historical_data, aes(x = year, y = !!sym(variable)), color = "blue") +
    geom_line(data = ensemble_predictions, aes(x = Year, y = Ensemble_Predictions), color = "red") +
    labs(title = paste("Historical", variable, "Data and Ensemble Predictions for", iso3_country),
         x = "Year",
         y = variable,
         color = "Data Type") +
    scale_color_manual(values = c("blue", "red"),
                       labels = c("Historical Data", "Ensemble Predictions")) +
    theme_minimal()
}

# Iterate through variables and make predictions and plot
variables <- c("co2_emissions_in_tonnes", "air_quality_in_mg_per_m3", "land_area_in_1000_ha", "forest_area_in_1000_ha", "carbon_stocks_in_millions_tonnes", "ghg_emissions_in_million_metric_tonnes_of_co2_equivalents_per_yr")

for (variable in variables) {
  predict_and_plot(variable)
}
