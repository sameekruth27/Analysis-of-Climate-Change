# Load necessary libraries
library(dplyr)

# Read the dataset
data <- read.csv("your_dataset.csv")  # Replace 'your_dataset.csv' with the path to your dataset

# Select air quality values for a specific country by ISO3 value
iso3_country <- "USA"  # Replace 'USA' with the ISO3 value of the desired country
selected_data <- data %>%
  filter(iso3 == iso3_country)

# Check the structure of selected data
str(selected_data)

# Build a linear regression model for year and air quality value
linear_model <- lm(air_quality_in_mg_per_m3 ~ year, data = selected_data)

# Summary of the linear regression model
summary(linear_model)

# Predict air quality value for a given year
year_to_predict <- 2025  # Replace '2025' with the year you want to predict
predicted_air_quality <- predict(linear_model, newdata = data.frame(year = year_to_predict))

# Print the predicted air quality value
print(predicted_air_quality)
