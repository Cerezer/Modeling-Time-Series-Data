# Install the packages if not already installed
# install.packages(c("forecast", "prophet", "ggplot2"))

# Load necessary libraries
library(forecast)  # For ARIMA modeling
library(prophet)   # For Prophet modeling
library(ggplot2)   # For data visualization

# Set seed for reproducibility
set.seed(123)

# Simulate Fake Time Series Data
n <- 60  # 60 months (5 years)
time <- seq(1, n)
seasonality <- 10 * sin(2 * pi * time / 12)  # Seasonal component
trend <- 0.5 * time  # Trend component
noise <- rnorm(n, mean = 0, sd = 5)  # Random noise
sales <- trend + seasonality + noise  # Combine components

# Create a time series object
sales_ts <- ts(sales, start = c(2018, 1), frequency = 12)

# Plot the simulated sales data
autoplot(sales_ts) + 
  ggtitle("Simulated Monthly Sales Data") + 
  ylab("Sales") + xlab("Time (Months)")

# ARIMA Modeling
fit_arima <- auto.arima(sales_ts)  # Automatically fit ARIMA model
summary(fit_arima)  # Display model details

# Forecast the next 12 months using ARIMA
forecast_arima <- forecast(fit_arima, h = 12)

# Plot ARIMA forecast
autoplot(forecast_arima) + 
  ggtitle("ARIMA Model Forecast") + 
  ylab("Sales") + xlab("Time (Months)")

# Prepare data for Prophet
sales_df <- data.frame(ds = seq.Date(from = as.Date("2018-01-01"), 
                                     by = "month", length.out = n), 
                       y = as.numeric(sales_ts))

# Fit the Prophet model
model_prophet <- prophet(sales_df)

# Create future dataframe for forecasting
future <- make_future_dataframe(model_prophet, periods = 12, freq = "month")
forecast_prophet <- predict(model_prophet, future)

# Plot Prophet forecast
plot(model_prophet, forecast_prophet) + 
  ggtitle("Prophet Model Forecast")

# Split data into training (first 4 years) and test (last year)
train_ts <- window(sales_ts, end = c(2021, 12))  # Training: 2018-2021
test_ts <- window(sales_ts, start = c(2022, 1))  # Test: 2022 onwards

# Fit ARIMA on the training set and forecast on the test set
fit_arima_train <- auto.arima(train_ts)
forecast_arima_test <- forecast(fit_arima_train, h = length(test_ts))

# Calculate and print accuracy metrics
accuracy_metrics <- accuracy(forecast_arima_test, test_ts)
print(accuracy_metrics)

# Combine actual test data and forecasted data into a single dataframe
plot_data <- data.frame(
  Date = time(test_ts),                       # Dates from the test data
  Actual = as.numeric(test_ts),               # Actual sales values
  Forecast = as.numeric(forecast_arima_test$mean),  # Forecasted values
  Lower_80 = as.numeric(forecast_arima_test$lower[, 1]),  # 80% CI lower bound
  Upper_80 = as.numeric(forecast_arima_test$upper[, 1]),  # 80% CI upper bound
  Lower_95 = as.numeric(forecast_arima_test$lower[, 2]),  # 95% CI lower bound
  Upper_95 = as.numeric(forecast_arima_test$upper[, 2])   # 95% CI upper bound
)

# Plot actual vs forecast with confidence intervals
ggplot(plot_data, aes(x = Date)) + 
  geom_line(aes(y = Actual, color = "Actual")) +  # Plot actual data
  geom_line(aes(y = Forecast, color = "Forecast")) +  # Plot forecast data
  geom_ribbon(aes(ymin = Lower_95, ymax = Upper_95), fill = "blue", alpha = 0.2) +  # 95% CI
  geom_ribbon(aes(ymin = Lower_80, ymax = Upper_80), fill = "blue", alpha = 0.4) +  # 80% CI
  ggtitle("Actual vs Forecasted Sales with Confidence Intervals") + 
  ylab("Sales") + 
  xlab("Time (Months)") +
  scale_color_manual(name = "Legend", values = c("Actual" = "black", "Forecast" = "red"))
