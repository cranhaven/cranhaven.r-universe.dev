



auxiliary_forecasting_function_1 <- function(series, max_lag = 1) {


  # Number of rows and columns of the MTS

  n_uts <- ncol(series)
  series_length <- nrow(series)


  # Creating a list with the empty lag-embedded matrices

  list_matrices <- list()

  for (i in 1 : n_uts) {

    list_matrices[[i]] <- matrix(0, nrow = series_length - max_lag, ncol = n_uts * max_lag + 1)

  }


  # Creating the common matrix

  common_matrix <- matrix(0, nrow = series_length - max_lag, ncol = n_uts * max_lag)

  for (i in (1 : (series_length - max_lag))) {

    common_matrix[i,] <- c(series[(i : (i + max_lag - 1)),])

  }


  # Creating the lag-embedded matrices

  lag_embedded_matrices <- list()
  for (i in 1 : n_uts) {

    lag_embedded_matrices[[i]] <- cbind(common_matrix, series[,i][(max_lag + 1) : series_length])

  }



  return(lag_embedded_matrices)


}


auxiliary_forecasting_function_2 <- function(list_matrices, model_caret = 'lm') {


  # Constructing Machine Learning models over the lag-embedded matrix
  # via the caret library

  n_matrices <- length(list_matrices)
  last_col <- ncol(list_matrices[[1]])
  trControl <- caret::trainControl(method = "none")

  list_models <- list()

  for (i in 1 : n_matrices) {

    train_explanatory <- data.frame(list_matrices[[i]][, 1 : (last_col - 1)])
    train_response <- list_matrices[[i]][, last_col]

    if (ncol(train_explanatory) == 1) {colnames(train_explanatory) <- 'X1'}
    list_models[[i]] <- caret::train(train_explanatory, train_response, method = model_caret,
                              trControl = trControl)
  }

  return(list_models)

}



auxiliary_forecasting_function_3 <- function(series, list_matrices, list_models, h = 1) {

  n_series <- length(list_matrices)
  max_lag <- (ncol(list_matrices[[1]])-1)/n_series
  series_length <- nrow(series)


  # Initial predictors

  initial_predictors <- series[(series_length - max_lag + 1) : series_length,]


  # Computing the predictions

  predictions_matrix <- matrix(0, nrow = h, ncol = n_series)
  auxiliary_predictors <- initial_predictors

  for (i in 1 : h){

    for (j in 1 : n_series) {

      df_predictions <- data.frame(t(c(auxiliary_predictors)))
      predictions_matrix[i, j] <- stats::predict(list_models[[j]], df_predictions)

    }

    if (is.matrix(auxiliary_predictors)) {

      auxiliary_predictors <- rbind(auxiliary_predictors[-1,], predictions_matrix[i,])

    } else {

      auxiliary_predictors <- predictions_matrix[i,]

    }



  }

  return(predictions_matrix)

}



auxiliary_forecasting_function_4 <- function(series, max_lag = 1, model_caret = 'lm', h = 1) {


  # Obtaining the list of lag-embedded matrices and the list of models

  list_matrices <- auxiliary_forecasting_function_1(series, max_lag = max_lag)
  list_models <- auxiliary_forecasting_function_2(list_matrices, model_caret = model_caret)


  # Obtaining the matrix with the h-step-ahead forecast

  predictions <- auxiliary_forecasting_function_3(series, list_matrices, list_models, h = h)

  return(predictions)

}
