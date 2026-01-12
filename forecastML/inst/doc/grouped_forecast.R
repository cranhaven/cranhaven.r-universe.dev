## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(fig.width = 7.15, fig.height = 4, warning = FALSE, message = FALSE)
knitr::opts_knit$set(fig.width = 7.15, fig.height = 4, warning = FALSE, message = FALSE)

## ---- message = FALSE, warning = FALSE----------------------------------------
library(forecastML)
library(dplyr)
library(DT)
library(ggplot2)
library(xgboost)

data("data_buoy_gaps", package = "forecastML")

DT::datatable(head(data_buoy_gaps), options = list(scrollX = TRUE))

## -----------------------------------------------------------------------------
data <- forecastML::fill_gaps(data_buoy_gaps, date_col = 1, frequency = '1 day', 
                              groups = 'buoy_id', static_features = c('lat', 'lon'))

print(list(paste0("The original dataset with gaps in data collection is ", nrow(data_buoy_gaps), " rows."), 
      paste0("The modified dataset with no gaps in data collection from fill_gaps() is ", nrow(data), " rows.")))

## -----------------------------------------------------------------------------
data$day <- lubridate::mday(data$date)
data$year <- lubridate::year(data$date)

## ---- message = FALSE, warning = FALSE----------------------------------------
p <- ggplot(data, aes(x = date, y = wind_spd, color = ordered(buoy_id), group = year))
p <- p + geom_line()
p <- p + facet_wrap(~ ordered(buoy_id), scales = "fixed")
p <- p + theme_bw() + theme(
  legend.position = "none"
) + xlab(NULL)
p

## -----------------------------------------------------------------------------
data$buoy_id <- as.numeric(factor(data$buoy_id))

## -----------------------------------------------------------------------------
outcome_col <- 1  # The column position of our 'wind_spd' outcome (after removing the 'date' column).

horizons <- c(1, 7, 30)  # Forecast 1, 1:7, and 1:30 days into the future.

lookback <- c(1:30, 360:370)  # Features from 1 to 30 days in the past and annually.

dates <- data$date  # Grouped time series forecasting requires dates.
data$date <- NULL  # Dates, however, don't need to be in the input data.

frequency <- "1 day"  # A string that works in base::seq(..., by = "frequency").

dynamic_features <- c("day", "year")  # Features that change through time but which will not be lagged.

groups <- "buoy_id"  # 1 forecast for each group or buoy.

static_features <- c("lat", "lon")  # Features that do not change through time.

## -----------------------------------------------------------------------------
type <- "train"  # Create a model-training dataset.

data_train <- forecastML::create_lagged_df(data, type = type, outcome_col = outcome_col,
                                           horizons = horizons, lookback = lookback,
                                           dates = dates, frequency = frequency,
                                           dynamic_features = dynamic_features,
                                           groups = groups, static_features = static_features, 
                                           use_future = FALSE)

DT::datatable(head(data_train$horizon_1), options = list(scrollX = TRUE))

## ---- message = FALSE, warning = FALSE----------------------------------------
p <- plot(data_train)  # plot.lagged_df() returns a ggplot object.
p <- p + geom_tile(NULL)  # Remove the gray border for a cleaner plot.
p

## ---- message = FALSE, warning = FALSE----------------------------------------
windows <- forecastML::create_windows(data_train, window_length = 365, skip = 730,
                                      include_partial_window = FALSE)

p <- plot(windows, data_train) + theme(legend.position = "none")
p

## ---- message = FALSE, warning = FALSE----------------------------------------
p <- plot(windows, data_train, group_filter = "buoy_id == 1") + 
  theme(legend.position = "none")
p

## -----------------------------------------------------------------------------
# The value of outcome_col can also be set in train_model() with train_model(outcome_col = 1).
model_function <- function(data, outcome_col = 1) {
  
  # xgboost cannot handle missing outcomes data.
  data <- data[!is.na(data[, outcome_col]), ]

  indices <- 1:nrow(data)
  
  set.seed(224)
  train_indices <- sample(1:nrow(data), ceiling(nrow(data) * .8), replace = FALSE)
  test_indices <- indices[!(indices %in% train_indices)]

  data_train <- xgboost::xgb.DMatrix(data = as.matrix(data[train_indices, 
                                                           -(outcome_col), drop = FALSE]),
                                     label = as.matrix(data[train_indices, 
                                                            outcome_col, drop = FALSE]))

  data_test <- xgboost::xgb.DMatrix(data = as.matrix(data[test_indices, 
                                                          -(outcome_col), drop = FALSE]),
                                    label = as.matrix(data[test_indices, 
                                                           outcome_col, drop = FALSE]))

  params <- list("objective" = "reg:linear")
  watchlist <- list(train = data_train, test = data_test)
  
  set.seed(224)
  model <- xgboost::xgb.train(data = data_train, params = params, 
                              max.depth = 8, nthread = 2, nrounds = 30,
                              metrics = "rmse", verbose = 0, 
                              early_stopping_rounds = 5, 
                              watchlist = watchlist)

  return(model)
}

## -----------------------------------------------------------------------------
#future::plan(future::multiprocess)  # Multi-core or multi-session parallel training.

model_results_cv <- forecastML::train_model(lagged_df = data_train,
                                            windows = windows,
                                            model_name = "xgboost",
                                            model_function = model_function, 
                                            use_future = FALSE)

## -----------------------------------------------------------------------------
summary(model_results_cv$horizon_1$window_1$model)

## -----------------------------------------------------------------------------
# If 'model' is passed as a named list, the prediction model would be accessed with model$model or model["model"].
prediction_function <- function(model, data_features) {
  x <- xgboost::xgb.DMatrix(data = as.matrix(data_features))
  data_pred <- data.frame("y_pred" = predict(model, x),
                          "y_pred_lower" = predict(model, x) - 2,  # Optional; in practice, forecast bounds are not hard coded.
                          "y_pred_upper" = predict(model, x) + 2)  # Optional; in practice, forecast bounds are not hard coded.
  return(data_pred)
}

## -----------------------------------------------------------------------------
data_pred_cv <- predict(model_results_cv, prediction_function = list(prediction_function), data = data_train)

## ---- message = FALSE, warning = FALSE----------------------------------------
plot(data_pred_cv) + theme(legend.position = "none")

## ---- message = FALSE, warning = FALSE----------------------------------------
plot(data_pred_cv, facet = group ~ model, group_filter = "buoy_id %in% c(1, 2, 3)", windows = 1) 

## ---- message = FALSE, warning = FALSE----------------------------------------
plot(data_pred_cv, facet = group ~ horizon, group_filter = "buoy_id %in% c(1, 2, 3)", windows = 1) 

## ---- message = FALSE, warning = FALSE----------------------------------------
data_error <- forecastML::return_error(data_pred_cv)

plot(data_error, type = "window", group_filter = "buoy_id %in% c(1, 2, 3)", metric = "mae")
plot(data_error, type = "horizon", group_filter = "buoy_id %in% c(1, 2, 3)", metric = "mae")
plot(data_error, type = "global", group_filter = "buoy_id %in% c(1, 2, 3)", metric = "mae")

## -----------------------------------------------------------------------------
type <- "forecast"  # Create a forecasting dataset for our predict() function.

data_forecast <- forecastML::create_lagged_df(data, type = type, outcome_col = outcome_col,
                                              horizons = horizons, lookback = lookback,
                                              dates = dates, frequency = frequency,
                                              dynamic_features = dynamic_features,
                                              groups = groups, static_features = static_features, 
                                              use_future = FALSE)

DT::datatable(head(data_forecast$horizon_1), options = list(scrollX = TRUE))

## -----------------------------------------------------------------------------
for (i in seq_along(data_forecast)) {
  data_forecast[[i]]$day <- lubridate::mday(data_forecast[[i]]$index)  # When dates are given, the 'index` is date-based.
  data_forecast[[i]]$year <- lubridate::year(data_forecast[[i]]$index)
}

## -----------------------------------------------------------------------------
data_forecasts <- predict(model_results_cv, prediction_function = list(prediction_function), data = data_forecast)

## ---- message = FALSE, warning = FALSE----------------------------------------
plot(data_forecasts)

## ---- message = FALSE, warning = FALSE----------------------------------------
plot(data_forecasts, facet = group ~ ., group_filter = "buoy_id %in% 1:3")

## ---- message = FALSE, warning = FALSE----------------------------------------
windows <- forecastML::create_windows(data_train, window_length = 0)

p <- plot(windows, data_train) + theme(legend.position = "none")
p

## -----------------------------------------------------------------------------
# Un-comment the code below and set 'use_future' to TRUE.
#future::plan(future::multiprocess)

model_results_no_cv <- forecastML::train_model(lagged_df = data_train, 
                                               windows = windows,
                                               model_name = "xgboost",
                                               model_function = model_function,
                                               use_future = FALSE)

## -----------------------------------------------------------------------------
data_forecasts <- predict(model_results_no_cv, prediction_function = list(prediction_function), data = data_forecast)

DT::datatable(head(data_forecasts), options = list(scrollX = TRUE))

## ---- message = FALSE, warning = FALSE----------------------------------------
data_combined <- forecastML::combine_forecasts(data_forecasts)

# Plot a background dataset of actuals using the most recent data.
data_actual <- data[dates >= as.Date("2018-11-01"), ]
actual_indices <- dates[dates >= as.Date("2018-11-01")]

# Plot all final forecasts plus historical data.
plot(data_combined, data_actual = data_actual, actual_indices = actual_indices)

## ---- message = FALSE, warning = FALSE----------------------------------------
plot(data_combined, data_actual = data_actual, actual_indices = actual_indices, 
     facet = group ~ ., group_filter = "buoy_id %in% c(1, 11, 12)")

## -----------------------------------------------------------------------------
# Plot final forecasts for a single buoy plus historical data.
plot(data_combined, data_actual = data_actual, actual_indices = actual_indices,
     group_filter = "buoy_id == 10")

