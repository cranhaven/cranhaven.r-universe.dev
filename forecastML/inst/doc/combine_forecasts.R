## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(fig.width = 7.15, fig.height = 4)
knitr::opts_knit$set(fig.width = 7.15, fig.height = 4)

## ---- warning = FALSE, message = FALSE----------------------------------------
# library(forecastML)
library(dplyr)
library(ggplot2)
library(glmnet)

data("data_seatbelts", package = "forecastML")
data <- data_seatbelts

## -----------------------------------------------------------------------------
horizons <- c(1, 3, 6, 9, 12)
data_train <- forecastML::create_lagged_df(data_seatbelts, type = "train", method = "direct",
                                           outcome_col = 1, lookback = 1:15, horizon = horizons)

windows <- forecastML::create_windows(data_train, window_length = 0)

model_fun <- function(data) {
  x <- as.matrix(data[, -1, drop = FALSE])
  y <- as.matrix(data[, 1, drop = FALSE])
  set.seed(1)
  model <- glmnet::cv.glmnet(x, y, nfolds = 5)
}

model_results <- forecastML::train_model(data_train, windows, model_name = "LASSO", model_function = model_fun)

prediction_fun <- function(model, data_features) {
  data_pred <- data.frame("y_pred" = predict(model, as.matrix(data_features)),
                          "y_pred_lower" = predict(model, as.matrix(data_features)) - 30,
                          "y_pred_upper" = predict(model, as.matrix(data_features)) + 30)
}

data_forecast <- forecastML::create_lagged_df(data_seatbelts, type = "forecast", method = "direct",
                                              outcome_col = 1, lookback = 1:15, horizon = horizons)

data_forecasts <- predict(model_results, prediction_function = list(prediction_fun), data = data_forecast)

data_forecasts <- forecastML::combine_forecasts(data_forecasts, type = "horizon")

plot(data_forecasts, data_actual = data_seatbelts[-(1:170), ], actual_indices = (1:nrow(data_seatbelts))[-(1:170)])

## -----------------------------------------------------------------------------
# LASSO

horizons <- c(1, 3, 6)
data_train <- forecastML::create_lagged_df(data_seatbelts, type = "train", method = "direct",
                                           outcome_col = 1, lookback = 1:15, horizon = horizons)

windows <- forecastML::create_windows(data_train, window_length = 0)

model_fun_lasso <- function(data) {
  x <- as.matrix(data[, -1, drop = FALSE])
  y <- as.matrix(data[, 1, drop = FALSE])
  set.seed(1)
  model <- glmnet::cv.glmnet(x, y, alpha = 1, nfolds = 5)
}

model_results <- forecastML::train_model(data_train, windows, model_name = "LASSO", model_function = model_fun_lasso)

prediction_fun <- function(model, data_features) {
  data_pred <- data.frame("y_pred" = predict(model, as.matrix(data_features)),
                          "y_pred_lower" = predict(model, as.matrix(data_features)) - 30,
                          "y_pred_upper" = predict(model, as.matrix(data_features)) + 30)
}

data_forecast <- forecastML::create_lagged_df(data_seatbelts, type = "forecast", method = "direct",
                                              outcome_col = 1, lookback = 1:15, horizon = horizons)

data_forecasts_lasso <- predict(model_results, prediction_function = list(prediction_fun), data = data_forecast)
#------------------------------------------------------------------------------
# Ridge

horizons <- c(9, 12)
data_train <- forecastML::create_lagged_df(data_seatbelts, type = "train", method = "direct",
                                           outcome_col = 1, lookback = 1:15, horizon = horizons)

windows <- forecastML::create_windows(data_train, window_length = 0)

model_fun_ridge <- function(data) {
  x <- as.matrix(data[, -1, drop = FALSE])
  y <- as.matrix(data[, 1, drop = FALSE])
  set.seed(1)
  model <- glmnet::cv.glmnet(x, y, alpha = 0, nfolds = 5)
}

model_results <- forecastML::train_model(data_train, windows, model_name = "Ridge", model_function = model_fun_ridge)

prediction_fun <- function(model, data_features) {
  data_pred <- data.frame("y_pred" = predict(model, as.matrix(data_features)),
                          "y_pred_lower" = predict(model, as.matrix(data_features)) - 30,
                          "y_pred_upper" = predict(model, as.matrix(data_features)) + 30)
}

data_forecast <- forecastML::create_lagged_df(data_seatbelts, type = "forecast", method = "direct",
                                              outcome_col = 1, lookback = 1:15, horizon = horizons)

data_forecasts_ridge <- predict(model_results, prediction_function = list(prediction_fun), data = data_forecast)
#------------------------------------------------------------------------------
# Forecast combination.

data_forecasts <- forecastML::combine_forecasts(data_forecasts_lasso, data_forecasts_ridge, type = "horizon")

plot(data_forecasts, data_actual = data_seatbelts[-(1:170), ], actual_indices = (1:nrow(data_seatbelts))[-(1:170)])

## -----------------------------------------------------------------------------
# LASSO

horizons <- c(1, 3, 6, 9, 12)
data_train <- forecastML::create_lagged_df(data_seatbelts, type = "train", method = "direct",
                                           outcome_col = 1, lookback = 1:15, horizon = horizons)

windows <- forecastML::create_windows(data_train, window_length = 0)

model_fun_lasso <- function(data) {
  x <- as.matrix(data[, -1, drop = FALSE])
  y <- as.matrix(data[, 1, drop = FALSE])
  set.seed(1)
  model <- glmnet::cv.glmnet(x, y, alpha = 1, nfolds = 5)
}

model_results <- forecastML::train_model(data_train, windows, model_name = "LASSO", model_function = model_fun_lasso)

prediction_fun <- function(model, data_features) {
  data_pred <- data.frame("y_pred" = predict(model, as.matrix(data_features)),
                          "y_pred_lower" = predict(model, as.matrix(data_features)) - 30,
                          "y_pred_upper" = predict(model, as.matrix(data_features)) + 30)
}

data_forecast <- forecastML::create_lagged_df(data_seatbelts, type = "forecast", method = "direct",
                                              outcome_col = 1, lookback = 1:15, horizon = horizons)

data_forecasts_lasso <- predict(model_results, prediction_function = list(prediction_fun), data = data_forecast)
#------------------------------------------------------------------------------
# Ridge

horizons <- c(1, 3, 6, 9, 12)
data_train <- forecastML::create_lagged_df(data_seatbelts, type = "train", method = "direct",
                                           outcome_col = 1, lookback = 1:15, horizon = horizons)

windows <- forecastML::create_windows(data_train, window_length = 0)

model_fun_ridge <- function(data) {
  x <- as.matrix(data[, -1, drop = FALSE])
  y <- as.matrix(data[, 1, drop = FALSE])
  set.seed(1)
  model <- glmnet::cv.glmnet(x, y, alpha = 0, nfolds = 5)
}

model_results <- forecastML::train_model(data_train, windows, model_name = "Ridge", model_function = model_fun_ridge)

prediction_fun <- function(model, data_features) {
  data_pred <- data.frame("y_pred" = predict(model, as.matrix(data_features)),
                          "y_pred_lower" = predict(model, as.matrix(data_features)) - 30,
                          "y_pred_upper" = predict(model, as.matrix(data_features)) + 30)
}

data_forecast <- forecastML::create_lagged_df(data_seatbelts, type = "forecast", method = "direct",
                                              outcome_col = 1, lookback = 1:15, horizon = horizons)

data_forecasts_ridge <- predict(model_results, prediction_function = list(prediction_fun), data = data_forecast)
#------------------------------------------------------------------------------
# Forecast combination.

data_forecasts <- forecastML::combine_forecasts(data_forecasts_lasso, data_forecasts_ridge,
                                                type = "horizon", aggregate = stats::median)

plot(data_forecasts, data_actual = data_seatbelts[-(1:170), ], actual_indices = (1:nrow(data_seatbelts))[-(1:170)])

