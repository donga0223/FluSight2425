# Load required libraries
library(tidyverse)
library(forecast)
library(mgcv)
library(prophet)
library(ggplot2)
library(zoo)
library(lightgbm)

# Epimodulation function
epimodulate_forecast <- function(forecast, theta) {
  if (is.null(theta) || !is.numeric(theta)) {
    stop("Error: `theta` must be a numeric value.")
  }
  adjusted_forecast <- forecast * exp(-theta * cumsum(forecast))
  pmax(adjusted_forecast, 0)
}

# Function: fit_ensemble() - Fit historical forecasts and optimize Negative Binomial theta
fit_ensemble <- function(y_train, forecast_horizon) {
  window_size <- floor(length(y_train) / 4)
  historical_forecasts <- list()
  features_list <- list()
  
  for (i in 1:(window_size - forecast_horizon)) {
    print(paste("Iteration:", i))
    training_data <- y_train[1:(i * 4)]
    
    # Generate forecasts for models
    arima_forecast <- tryCatch(forecast(auto.arima(training_data), h = forecast_horizon)$mean, error = function(e) rep(NA, forecast_horizon))
    sarima_forecast <- tryCatch(forecast(auto.arima(training_data, seasonal = TRUE), h = forecast_horizon)$mean, error = function(e) rep(NA, forecast_horizon))
    ets_forecast <- tryCatch(forecast(ets(training_data), h = forecast_horizon)$mean, error = function(e) rep(NA, forecast_horizon))
    tbats_forecast <- tryCatch(forecast(tbats(training_data), h = forecast_horizon)$mean, error = function(e) rep(NA, forecast_horizon))
    nnar_forecast <- tryCatch(forecast(nnetar(training_data), h = forecast_horizon)$mean, error = function(e) rep(NA, forecast_horizon))
    
    t_final <- seq_along(training_data)
    spline_forecast <- tryCatch(predict(mgcv::gam(training_data ~ s(t_final, k = length(training_data) / 4)),
                                        newdata = data.frame(t_final = seq(length(training_data) + 1, length(training_data) + forecast_horizon))), 
                                error = function(e) rep(NA, forecast_horizon))
    
    prophet_data <- data.frame(ds = as.Date(seq_along(training_data), origin = "1970-01-01"), y = training_data)
    prophet_forecast <- tryCatch({
      prophet_model <- prophet(prophet_data)
      future <- data.frame(ds = as.Date(seq(length(training_data) + 1, length(training_data) + forecast_horizon), origin = "1970-01-01"))
      tail(training_data, 1) + cumsum(predict(prophet_model, future)$yhat)
    }, error = function(e) rep(NA, forecast_horizon))
    
    # Combine forecasts into historical_forecasts
    true_values <- y_train[((i * 4) + 1):((i * 4) + forecast_horizon)]
    if (length(true_values) != forecast_horizon) true_values <- rep(NA, forecast_horizon)
    
    historical_forecasts[[i]] <- list(
      arima = arima_forecast,
      sarima = sarima_forecast,
      ets = ets_forecast,
      tbats = tbats_forecast,
      nnar = nnar_forecast,
      spline = spline_forecast,
      prophet = prophet_forecast,
      true_values = true_values
    )
  }
  
  # Function to calculate Negative Binomial log-likelihood
  negbin_loglik <- function(theta, observed, predicted) {
    mean_val <- predicted
    variance <- mean_val + (mean_val^2) / theta
    alpha <- mean_val^2 / (variance - mean_val)
    likelihoods <- dnbinom(observed, size = alpha, mu = mean_val, log = TRUE)
    return(-sum(likelihoods, na.rm = TRUE))  # Negative log-likelihood
  }
  
  # Optimize theta for overdispersion
  combined_forecasts <- do.call(rbind, lapply(historical_forecasts, function(x) data.frame(predicted = rowMeans(data.frame(x[-length(x)]), na.rm = TRUE), observed = x$true_values)))
  theta_opt <- optim(par = 1, fn = function(theta) negbin_loglik(theta, combined_forecasts$observed, combined_forecasts$predicted),
                     method = "L-BFGS-B", lower = 0.1, upper = 100)
  
  theta <- theta_opt$par
  print(paste("Optimized theta:", theta))
  
  return(list(historical_forecasts = historical_forecasts, theta = theta))
}

# Function: predict_ensemble() - Generate forecasts with Negative Binomial prediction intervals
predict_ensemble <- function(y_train, fit_result, forecast_horizon) {
  theta <- 10*fit_result$theta
  historical_forecasts <- fit_result$historical_forecasts
  
  # Generate ensemble forecast
  arima_forecast <- forecast(auto.arima(y_train), h = forecast_horizon)$mean
  sarima_forecast <- forecast(auto.arima(y_train, seasonal = TRUE), h = forecast_horizon)$mean
  ets_forecast <- forecast(ets(y_train), h = forecast_horizon)$mean
  tbats_forecast <- forecast(tbats(y_train), h = forecast_horizon)$mean
  nnar_forecast <- forecast(nnetar(y_train), h = forecast_horizon)$mean
  
  t_final <- seq_along(y_train)
  spline_forecast <- predict(mgcv::gam(y_train ~ s(t_final, k = length(y_train) / 4)),
                             newdata = data.frame(t_final = seq(length(y_train) + 1, length(y_train) + forecast_horizon)))
  
  prophet_data <- data.frame(ds = as.Date(seq_along(y_train), origin = "1970-01-01"), y = y_train)
  prophet_model <- prophet(prophet_data)
  future <- data.frame(ds = as.Date(seq(length(y_train) + 1, length(y_train) + forecast_horizon), origin = "1970-01-01"))
  prophet_forecast <- tail(y_train, 1) + cumsum(predict(prophet_model, future)$yhat)
  
  adjusted_forecasts <- rowMeans(data.frame(arima_forecast, sarima_forecast, ets_forecast, tbats_forecast, nnar_forecast, spline_forecast, prophet_forecast), na.rm = TRUE)
  quantiles <- c(0.01, 0.025, seq(0.05, 0.95, by = 0.05), 0.975, 0.99)
  res <- matrix(NA, nrow = n_horizon, ncol = length(quantiles))
  for(i in 1:length(quantiles)){
    res[,i] <- qnbinom(quantiles[i], size = theta, mu = adjusted_forecasts)
  }
  colnames(res) <- as.character(quantiles)
  rownames(res) <- as.character(0:3)
  res_melt <- melt(res)
  
  res_melt <- res_melt %>% 
    mutate(output_type = "quantile",
           target = "wk inc flu hosp") %>% 
    rename(horizon = Var1,
           output_type_id = Var2) 
  
  return(list(mean = adjusted_forecasts, quantiles = res_melt))
}