## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(fig.width = 7.15, fig.height = 4)
knitr::opts_knit$set(fig.width = 7.15, fig.height = 4)

## ---- eval = FALSE------------------------------------------------------------
#  install.packages("forecastML")

## ---- warning = FALSE, message = FALSE----------------------------------------
library(forecastML)
library(dplyr)
library(DT)
library(ggplot2)
library(glmnet)
library(randomForest)

data("data_seatbelts", package = "forecastML")
data <- data_seatbelts

date_frequency <- "1 month"  # Time step frequency.

# The date indices, which don't come with the stock dataset, should not be included in the modeling data.frame.
dates <- seq(as.Date("1969-01-01"), as.Date("1984-12-01"), by = date_frequency)

data$PetrolPrice <- round(data$PetrolPrice, 3)

data <- data[, c("DriversKilled", "kms", "PetrolPrice", "law")]
DT::datatable(head(data, 5))

## -----------------------------------------------------------------------------
data_train <- data[1:(nrow(data) - 12), ]
data_test <- data[(nrow(data) - 12 + 1):nrow(data), ]

## -----------------------------------------------------------------------------
p <- ggplot(data, aes(x = dates, y = DriversKilled))
p <- p + geom_line()
p <- p + geom_vline(xintercept = dates[nrow(data_train)], color = "red", size = 1.1)
p <- p + theme_bw() + xlab("Dataset index")
p

## -----------------------------------------------------------------------------
outcome_col <- 1  # The column index of our DriversKilled outcome.

horizons <- c(1, 3, 6, 12)  # 4 models that forecast 1, 1:3, 1:6, and 1:12 time steps ahead.

# A lookback across select time steps in the past. Feature lags 1 through 9, for instance, will be 
# silently dropped from the 12-step-ahead model.
lookback <- c(1:6, 9, 12, 15)

# A non-lagged feature that changes through time whose value we either know (e.g., month) or whose 
# value we would like to forecast.
dynamic_features <- "law"

data_list <- forecastML::create_lagged_df(data_train,
                                          outcome_col = outcome_col,
                                          type = "train",
                                          horizons = horizons,
                                          lookback = lookback,
                                          date = dates[1:nrow(data_train)],
                                          frequency = date_frequency,
                                          dynamic_features = dynamic_features
                                          )

## -----------------------------------------------------------------------------
DT::datatable(head(data_list$horizon_6, 10), options = list(scrollX = TRUE))

## -----------------------------------------------------------------------------
plot(data_list)

## ---- warnings = FALSE, message = FALSE---------------------------------------
windows <- forecastML::create_windows(lagged_df = data_list, window_length = 12, skip = 48,
                                      window_start = NULL, window_stop = NULL,
                                      include_partial_window = TRUE)
windows

## -----------------------------------------------------------------------------
plot(windows, data_list, show_labels = TRUE)

## -----------------------------------------------------------------------------
# Example 1 - LASSO
# Alternatively, we could define an outcome column identifier argument, say, 'outcome_col = 1' in 
# this function or just 'outcome_col' and then set the argument as 'outcome_col = 1' in train_model().
model_function <- function(data) {
  
  # The 'law' feature is constant during some of our outer-loop validation datasets so we'll 
  # simply drop it so that glmnet converges.
  constant_features <- which(unlist(lapply(data[, -1], function(x) {!(length(unique(x)) > 1)})))
  
  if (length(constant_features) > 1) {
    data <- data[, -c(constant_features + 1)]  # +1 because we're skipping over the outcome column.
  }

  x <- data[, -(1), drop = FALSE]
  y <- data[, 1, drop = FALSE]
  x <- as.matrix(x, ncol = ncol(x))
  y <- as.matrix(y, ncol = ncol(y))

  model <- glmnet::cv.glmnet(x, y, nfolds = 3)
  return(list("model" = model, "constant_features" = constant_features))
}

# Example 2 - Random Forest
# Alternatively, we could define an outcome column identifier argument, say, 'outcome_col = 1' in 
# this function or just 'outcome_col' and then set the argument as 'outcome_col = 1' in train_model().
model_function_2 <- function(data) {

  outcome_names <- names(data)[1]
  model_formula <- formula(paste0(outcome_names,  "~ ."))

  model <- randomForest::randomForest(formula = model_formula, data = data, ntree = 200)
  return(model)
}

## -----------------------------------------------------------------------------
#future::plan(future::multiprocess)

model_results <- forecastML::train_model(data_list, windows, model_name = "LASSO",
                                         model_function, use_future = FALSE)

model_results_2 <- forecastML::train_model(data_list, windows, model_name = "RF", 
                                           model_function_2, use_future = FALSE)

## -----------------------------------------------------------------------------
# Example 1 - LASSO.
prediction_function <- function(model, data_features) {
  
  if (length(model$constant_features) > 1) {  # 'model' was passed as a list.
    data_features <- data_features[, -c(model$constant_features )]
  }

  x <- as.matrix(data_features, ncol = ncol(data_features))

  data_pred <- data.frame("y_pred" = predict(model$model, x, s = "lambda.min"))
  return(data_pred)
}

# Example 2 - Random Forest.
prediction_function_2 <- function(model, data_features) {

  data_pred <- data.frame("y_pred" = predict(model, data_features))
  return(data_pred)
}

## ---- warning = FALSE, message = FALSE----------------------------------------
data_results <- predict(model_results, model_results_2,
                        prediction_function = list(prediction_function, prediction_function_2), 
                        data = data_list)

## -----------------------------------------------------------------------------
data_results$DriversKilled_pred <- round(data_results$DriversKilled_pred, 0)
DT::datatable(head(data_results, 30), options = list(scrollX = TRUE))

## -----------------------------------------------------------------------------
plot(data_results, type = "prediction", horizons = c(1, 6, 12))

## -----------------------------------------------------------------------------
plot(data_results, type = "residual", horizons = c(1, 6, 12))

## -----------------------------------------------------------------------------
plot(data_results, type = "forecast_stability", windows = 3)

## ---- warning = FALSE, message = FALSE----------------------------------------
data_error <- forecastML::return_error(data_results)

# Global error.
data_error$error_global[, -1] <- lapply(data_error$error_global[, -1], round, 1)

DT::datatable(data_error$error_global, options = list(scrollX = TRUE), )

## -----------------------------------------------------------------------------
plot(data_error, type = "window", facet = ~ horizon, horizons = c(1, 6, 12))

## -----------------------------------------------------------------------------
plot(data_error, type = "horizon", facet = ~ horizon, horizons = c(1, 6, 12))

## -----------------------------------------------------------------------------
plot(data_error, type = "global", facet = ~ horizon)

## -----------------------------------------------------------------------------
hyper_function <- function(model) {

  lambda_min <- model$model$lambda.min
  lambda_1se <- model$model$lambda.1se

  data_hyper <- data.frame("lambda_min" = lambda_min, "lambda_1se" = lambda_1se)
  return(data_hyper)
}

## ---- warning = FALSE, message = FALSE----------------------------------------
data_hyper <- forecastML::return_hyper(model_results, hyper_function)

plot(data_hyper, data_results, data_error, type = "stability", horizons = c(1, 6, 12))
plot(data_hyper, data_results, data_error, type = "error", c(1, 6, 12))

## -----------------------------------------------------------------------------
data_forecast_list <- forecastML::create_lagged_df(data_train,
                                                   outcome_col = outcome_col,
                                                   type = "forecast",
                                                   horizons = horizons,
                                                   lookback = lookback,
                                                   date = dates[1:nrow(data_train)],
                                                   frequency = date_frequency,
                                                   dynamic_features = dynamic_features
                                                   )

DT::datatable(head(data_forecast_list$horizon_6), options = list(scrollX = TRUE))

## -----------------------------------------------------------------------------
for (i in seq_along(data_forecast_list)) {
  data_forecast_list[[i]]$law <- 1
}

## ---- warning = FALSE, message = FALSE----------------------------------------
data_forecast <- predict(model_results, model_results_2,  # ... supports any number of ML models.
                         prediction_function = list(prediction_function, prediction_function_2), 
                         data = data_forecast_list)

data_forecast$DriversKilled_pred <- round(data_forecast$DriversKilled_pred, 0)

DT::datatable(head(data_forecast, 10), options = list(scrollX = TRUE))

## -----------------------------------------------------------------------------
plot(data_forecast,
     data_actual = data[-(1:150), ],  # Actuals from the training and test data sets.
     actual_indices = dates[-(1:150)], 
     horizons = c(1, 6, 12))

## -----------------------------------------------------------------------------
data_error <- forecastML::return_error(data_forecast,
                                       data_test = data_test,
                                       test_indices = dates[(nrow(data_train) + 1):length(dates)])

plot(data_error, facet = ~ horizon, type = "window")

plot(data_error, facet = ~ horizon, type = "horizon")

plot(data_error, facet = ~ horizon, type = "global")

## -----------------------------------------------------------------------------
data_list <- forecastML::create_lagged_df(data_train,
                                          outcome_col = outcome_col,
                                          type = "train",
                                          horizons = horizons,
                                          lookback = lookback,
                                          date = dates[1:nrow(data_train)],
                                          frequency = date_frequency,
                                          dynamic_features = dynamic_features
                                          )

## -----------------------------------------------------------------------------
windows <- forecastML::create_windows(data_list, window_length = 0)

plot(windows, data_list, show_labels = TRUE)

## -----------------------------------------------------------------------------
model_results <- forecastML::train_model(data_list, windows,  model_name = "LASSO", model_function)

data_results <- predict(model_results, prediction_function = list(prediction_function), data = data_list)

DT::datatable(head(data_results, 10), options = list(scrollX = TRUE))
plot(data_results, type = "prediction", horizons = c(1, 6, 12))

## ---- warning = FALSE, message = FALSE----------------------------------------
data_error <- forecastML::return_error(data_results)

data_error$error_global[, -1] <- lapply(data_error$error_global[, -1], round, 1)

DT::datatable(head(data_error$error_global), options = list(scrollX = TRUE))

## -----------------------------------------------------------------------------
data_forecast_list <- forecastML::create_lagged_df(data_train,
                                                   outcome_col = outcome_col,
                                                   type = "forecast",
                                                   horizons = horizons,
                                                   lookback = lookback,
                                                   date = dates[1:nrow(data_train)],
                                                   frequency = date_frequency,
                                                   dynamic_features = dynamic_features
                                                   )

for (i in seq_along(data_forecast_list)) {
  data_forecast_list[[i]]$law <- 1
}

data_forecast <- predict(model_results, prediction_function = list(prediction_function), data = data_forecast_list)

plot(data_forecast,
     data_actual = data[-(1:150), ],
     actual_indices = dates[-(1:150)],
     horizons = c(1, 6, 12))

## -----------------------------------------------------------------------------
data_error <- forecastML::return_error(data_forecast, data_test = data_test, 
                                       test_indices = dates[(nrow(data_train) + 1):nrow(data)])

plot(data_error, type = "horizon", facet = ~ horizon)

## ---- message = FALSE, warning = FALSE----------------------------------------
data_combined <- forecastML::combine_forecasts(data_forecast)

# Plot a background dataset of actuals using the most recent data.
data_actual <- data[dates >= as.Date("1980-01-01"), ]
actual_indices <- dates[dates >= as.Date("1980-01-01")]

# Plot all final forecasts plus historical data.
plot(data_combined, data_actual = data_actual, actual_indices = actual_indices)

# Error by forecast horizon.
DT::datatable(return_error(data_combined, data_actual, actual_indices)$error_by_horizon, 
              options = list(scrollX = TRUE))

# Error aggregated across forecast horizons.
DT::datatable(return_error(data_combined, data_actual, actual_indices)$error_global, 
              options = list(scrollX = TRUE))

