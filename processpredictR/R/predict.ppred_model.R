#' Generic predict function for ppred_model class.
#' @param object [`ppred_model`] (default [`NULL`]): ProcessTransformer model of class [`ppred_model`].
#' @param test_data [`ppred_examples_df`] (default [`NULL`]): preprocessed test data.
#' @param append [`logical`] (default [`FALSE`]): if [`TRUE`], returns a passed [`data.frame`] with predicted values.
#' @param ... Additional arguments
#'
#' @importFrom stats predict
#' @export
stats::predict

#' @export
predict.ppred_model <- function(object, test_data, output = c("append", "y_pred", "raw"), ...) {

  end_time <- NULL
  start_time <- NULL
  pred_start_ <- NULL
  actual_start_ <- NULL
  remaining_time <- NULL
  pred_ <- NULL
  actual_ <- NULL

  output <- rlang::arg_match(output)

  # if test_data is a preprocessed test dataset (before tokenize)
  if (any((test_data %>% class) == "ppred_examples_df")) {
    tokens_test <- test_data %>% tokenize()
  }

  # if test_data is already tokenized
  else if (any((test_data %>% class) == "ppred_examples_tokens")) {
    tokens_test <- test_data
  }

  # tokenized trace
  x_tokens_test <- tokens_test$token_x %>% keras::pad_sequences(maxlen = object$max_case_length, value = 0)

  # if (object$number_features > 0) {
  x_numeric_features <- tokens_test$numeric_features
  x_categorical_features <- tokens_test$categorical_features

  x_test_list <- list(x_tokens_test)
  if (!is.null(x_numeric_features)) x_test_list <- x_test_list %>% append(list(x_numeric_features))
  if (!is.null(x_categorical_features)) x_test_list <- x_test_list %>% append(list(x_categorical_features))

  y_pred <- stats::predict(object$model, x_test_list, ...)

# Postprocessing ----------------------------------------------------------
  # OUTCOME, NEXT_ACTIVITY & REMAINING_TRACE
  if (object$task %in% c("outcome", "next_activity", "remaining_trace")) {

    if (output == "raw") {
      colnames(y_pred) <- object$vocabulary$keys_y
      return(y_pred)
    }

    # IF output == y_pred || output == append
    # CONVERT NUMERIC Y_PRED TO ACTIVITY LABELS
    y_pred <- y_pred %>% keras::k_argmax(axis = -1) %>% as.numeric()
    vocabulary <- object$vocabulary
    vocabulary <- vocabulary$keys_y %>% unlist() %>% as_tibble() %>%
      mutate(key_y = row_number()) %>%
      rename_with(function(.x) {glue::glue("pred_{object$task}")}, "value")
      #rename_with(~ paste0(., object$task), value)


    if (output == "y_pred") {
      y_pred <- tibble(y_pred = as.numeric(y_pred) + 1) %>%
        left_join(vocabulary, by = c("y_pred" = "key_y")) %>%
        pull(paste0("pred_", object$task))
      return(y_pred)
    }

    else if (output == "append") {
      # append to test_data
      if (any((test_data %>% class) == "ppred_examples_df")) {
        test_data %>%
          mutate(y_pred = y_pred + 1) %>%
          left_join(vocabulary, by = c("y_pred" = "key_y")) -> y_pred
        class(y_pred) <- c("ppred_predictions", class(y_pred))
      }

      # create a tibble from tokenize() output
      else {
        tibble(y_pred = as.numeric(y_pred) + 1,
               y_actual = as.numeric(tokens_test$token_y) + 1) %>%
          left_join(vocabulary, by = c("y_pred" = "key_y")) %>%
          left_join(vocabulary, by = c("y_actual" = "key_y")) %>%
          setNames(c("y_pred", "y_actual", "pred_label", "actual_label")) -> y_pred
      }
    }
  }

  # NEXT_TIME & REMAINING_TIME
  else if (object$task %in% c("next_time", "remaining_time")) {
    ###########################    ###########################    ###########################    ###########################
    # # original
    # mean <- object$y_normalize_layer$mean %>% as.double()
    # variance <- object$y_normalize_layer$variance %>% as.double()
    # y_pred <- y_pred * sqrt(variance) + mean
    y_pred <- y_pred * object$sd_time
    ###########################    ###########################    ###########################    ###########################



    # NEXT_TIME
    if (object$task == "next_time") {
      if (output == "append") {
        if (any((test_data %>% class) == "ppred_examples_df")) {
          test_data %>% mutate(y_pred = as.numeric(y_pred)) %>%
            mutate(pred_start_ = end_time + y_pred,
                   actual_start_ = lead(start_time)) %>%
            rename_with(~ paste0(., object$task), c(pred_start_, actual_start_)) -> y_pred
          # as_tibble() %>% select(complete, actual_starttime_next_activity, predicted_starttime_next_activity) %>%
          # ggplotly(aes(predicted_starttime_next_activity, start2)) + geom_point()
        }
        else {
          tibble(pred_ = as.numeric(y_pred),
                 y_actual = as.numeric(tokens_test$token_y)) -> y_pred
        }
        class(y_pred) <- c("ppred_predictions", class(y_pred))
      }
    }


    # REMAINING_TIME
    else if (object$task == "remaining_time") {
      if (output == "append") {
        if (any((test_data %>% class) == "ppred_examples_df")) {
          test_data %>% mutate(y_pred = as.numeric(y_pred)) %>%
            mutate(pred_ = y_pred,
                   actual_ = remaining_time) %>%
            rename_with(~ paste0(., attr(tmp$test_df, "task")), c(pred_, actual_)) -> y_pred
          # as_tibble() %>% select(complete, actual_starttime_next_activity, predicted_starttime_next_activity) %>%
          # ggplotly(aes(predicted_starttime_next_activity, start2)) + geom_point()
        }
        else {
          tibble(y_pred = as.numeric(y_pred),
                 y_actual = as.numeric(tokens_test$token_y)) -> y_pred
        }
        class(y_pred) <- c("ppred_predictions", class(y_pred))
      }
    }
  }
  return(y_pred)
}

    # # NEXT_TIME & REMAINING_TIME
    # if (object$task %in% c("next_time", "remaining_time")) {
    #   mean <- object$y_normalize_layer$mean %>% as.double()
    #   variance <- object$y_normalize_layer$variance %>% as.double()
    #   y_pred <- y_pred * sqrt(variance) + mean
    #
    #   if (any((test_data %>% class) == "ppred_examples_df")) {
    #     test_data %>% mutate(y_pred = as.numeric(y_pred)) %>%
    #       mutate(predicted_starttime_next_activity = complete + y_pred,
    #              actual_starttime_next_activity = lead(start)) -> y_pred
    #     # as_tibble() %>% select(complete, actual_starttime_next_activity, predicted_starttime_next_activity) %>%
    #     # ggplotly(aes(predicted_starttime_next_activity, start2)) + geom_point()
    #   }
    #   else {
    #     tibble(y_pred = as.numeric(y_pred),
    #            y_actual = as.numeric(tokens_test$token_y)) -> y_pred
    #   }
    # }

    # # OUTCOME, NEXT_ACTIVITY & REMAINING_TRACE
    # else if (object$task %in% c("outcome", "next_activity", "remaining_trace")) {
    #   y_pred <- y_pred %>% keras::k_argmax(axis = -1) %>% as.numeric()
    #
    #   # vocabulary <- test_data %>% attr("vocabulary")
    #   vocabulary <- object$vocabulary
    #   vocabulary <- vocabulary$keys_y %>% unlist() %>% as_tibble() %>%
    #     mutate(key_y = row_number()) %>% rename(pred_label = "value")
    #
    #   if (any((test_data %>% class) == "ppred_examples_df")) {
    #     test_data %>%
    #       mutate(y_pred = y_pred + 1) %>%
    #       left_join(vocabulary, by = c("y_pred" = "key_y")) -> y_pred
    #   }
    #   else {
    #     tibble(y_pred = as.numeric(y_pred) + 1,
    #            y_actual = as.numeric(tokens_test$token_y) + 1) %>%
    #       left_join(vocabulary, by = c("y_pred" = "key_y")) %>%
    #       left_join(vocabulary, by = c("y_actual" = "key_y")) %>%
    #       setNames(c("y_pred", "y_actual", "pred_label", "actual_label")) -> y_pred
    #   }
    # }






