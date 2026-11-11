#' Generic evaluate function for ppred_model class.
#' @param object [`ppred_model`] (default [`NULL`]): ProcessTransformer model of class [`ppred_model`].
#' @param test_data [`ppred_examples_df`] (default [`NULL`]): preprocessed test data.
#' @param ... Additional arguments
#'
#' @importFrom keras evaluate
#' @export
keras::evaluate

#' @export
evaluate.ppred_model <- function(object, test_data, ...) {

  # if test_data is a preprocessed test dataset (before tokenize)
  if (any((test_data %>% class) == "ppred_examples_df")) {
    tokens_test <- test_data %>% tokenize()
  }

  # if test_data is already tokenized
  else if (any((test_data %>% class) == "ppred_examples_tokens")) {
    tokens_test <- test_data
  }

# x_test ------------------------------------------------------------------
  # tokenized traces
  x_tokens_test <- tokens_test$token_x %>% keras::pad_sequences(maxlen = object$max_case_length, value = 0)

  # extra numeric and categorical features (if present)
  x_numeric_features <- tokens_test$numeric_features
  x_categorical_features <- tokens_test$categorical_features
  x_test_list <- list(x_tokens_test)
  if (!is.null(x_numeric_features)) x_test_list <- x_test_list %>% append(list(x_numeric_features))
  if (!is.null(x_categorical_features)) x_test_list <- x_test_list %>% append(list(x_categorical_features))

# y_test ------------------------------------------------------------------
  y_tokens_test <- tokens_test$token_y

  # NEXT_TIME & REMAINING_TIME
  if (object$task %in% c("next_time", "remaining_time")) {

    metrics <- list(...)
    if(length(metrics) == 0) {

      #######################    #######################    #######################    #######################
      y_tokens_test <- y_tokens_test / object$sd_time

      # should be based solely on the metrics from compile()
      results <- keras::evaluate(object$model, x_test_list, y_tokens_test, ...) #%>% keras::k_argmax(axis = -1)
      return(results)

      # # original
      # mean <- object$y_normalize_layer$mean %>% as.double()
      # variance <- object$y_normalize_layer$variance %>% as.double()
      # y_tokens_test <- keras::layer_normalization(y_tokens_test, mean = mean, variance = variance)

      #######################    #######################    #######################    #######################
      # normalize_y <- keras::layer_normalization()
      # normalize_y %>% adapt(y_tokens_test)
      # y_tokens_test <- normalize_y(y_tokens_test)
    }

    else {
      results <- predict(object, test_data, output = "append")

      # for(i in 1:length(metrics)) {
      #   metrics[[i]](y_tokens_test, results)
      # }

      y_pred <- NULL
      y_var <- if_else(object$task == "next_time", "time_till_next_activity", "remaining_time")
       results %>% summarize(
        across(y_var,
               .fns = list(...),
               y_pred,
               .names = "metric_{.fn}"))

      # tmppred %>% summarize(across(time_till_next_activity,
      #                              .fns = list(mae = Metrics::mae, rmse = Metrics::rmse),
      #                              y_pred,
      #                              .names = "metric_{.fn}") )

    }
  }

  else {
    results <- keras::evaluate(object$model, x_test_list, y_tokens_test, ...) #%>% keras::k_argmax(axis = -1)
    return(results)
  }
}


#### PREDICT ######
# Metrics::rmse()


