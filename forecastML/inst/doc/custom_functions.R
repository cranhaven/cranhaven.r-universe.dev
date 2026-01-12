## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(fig.width = 7.15, fig.height = 4)

## ---- message = FALSE, warning = FALSE----------------------------------------
library(DT)
library(dplyr)
library(ggplot2)
library(forecastML)
library(randomForest)

data("data_seatbelts", package = "forecastML")
data <- data_seatbelts

data <- data[, c("DriversKilled", "kms", "PetrolPrice", "law")]

dates <- seq(as.Date("1969-01-01"), as.Date("1984-12-01"), by = "1 month")

## -----------------------------------------------------------------------------
data_train <- forecastML::create_lagged_df(data,
                                           type = "train",
                                           outcome_col = 1, 
                                           lookback = 1:12,
                                           horizons = c(3, 12),
                                           dates = dates,
                                           frequency = "1 month")

# View the horizon 3 lagged dataset.
DT::datatable(head((data_train$horizon_3)), options = list("scrollX" = TRUE))

## -----------------------------------------------------------------------------
windows <- forecastML::create_windows(data_train, window_length = 0, 
                                      window_start = as.Date("1984-01-01"),
                                      window_stop = as.Date("1984-12-01"))

plot(windows, data_train)

## -----------------------------------------------------------------------------
attributes(data_train$horizon_3)$horizon
attributes(data_train$horizon_12)$horizon

## -----------------------------------------------------------------------------
model_function <- function(data, my_outcome_col = 1, n_tree = c(200, 100)) {

  outcome_names <- names(data)[my_outcome_col]
  model_formula <- formula(paste0(outcome_names,  "~ ."))
  
  if (attributes(data)$horizon == 3) {  # Model 1
    
          model <- randomForest::randomForest(formula = model_formula, 
                                              data = data, 
                                              ntree = n_tree[1])
          
          return(list("my_trained_model" = model, "n_tree" = n_tree[1], 
                      "meta_data" = attributes(data)$horizon))
      
  } else if (attributes(data)$horizon == 12) {  # Model 2
    
          model <- randomForest::randomForest(formula = model_formula, 
                                              data = data, 
                                              ntree = n_tree[2])
          
          return(list("my_trained_model" = model, "n_tree" = n_tree[2],
                      "meta_data" = attributes(data)$horizon))
  }
}

## -----------------------------------------------------------------------------
model_results <- forecastML::train_model(data_train, windows, model_name = "RF", model_function)

## -----------------------------------------------------------------------------
model_results$horizon_3$window_1$model
model_results$horizon_12$window_1$model

## -----------------------------------------------------------------------------
prediction_function <- function(model, data_features) {
  
    if (model$meta_data == 3) {  # Perform a transformation specific to model 1.
      
        data_pred <- data.frame("y_pred" = predict(model$my_trained_model, data_features))
    }
  
    if (model$meta_data == 12) {  # Perform a transformation specific to model 2.
      
        data_pred <- data.frame("y_pred" = predict(model$my_trained_model, data_features))
    }

  return(data_pred)
}

## -----------------------------------------------------------------------------
data_results <- predict(model_results,
                        prediction_function = list(prediction_function),
                        data = data_train)

## -----------------------------------------------------------------------------
plot(data_results)

