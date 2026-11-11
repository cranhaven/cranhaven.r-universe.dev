#' @title Confusion matrix for predictions
#' @param predictions [`ppred_predictions`]: A [`data.frame`] with predicted values returned by `predict.ppred_model()`.
#' @param ... additional arguments.
#' @return A [`table`] object that can be used for plotting a confusion matrix using `plot()`.
#'
#'
#' @export
confusion_matrix <- function(predictions, ...) {
  UseMethod("confusion_matrix")
}

#' @export
confusion_matrix.ppred_predictions <- function(predictions, ...)  {

  y_var <- predictions %>% attr("y_var")
  task <- predictions %>% attr("task")
  if (task %in% c("outcome", "next_activity")) {
    output <- table(predictions[[y_var]], predictions[[paste0("pred_", task)]])
  }

  else {
    cli::cli_abort("Only applicable for tasks: outcome, next_activity")
  }

  output

  # # if test_data is a preprocessed test dataset (before tokenize)
  # if (any((test_data %>% class) == "ppred_examples_df")) {
  #   tokens_test <- test_data %>% tokenize()
  #   y_tokens_test <- tokens_test$token_y
  # }
  #
  # # if test_data is already tokenized
  # else if (any((test_data %>% class) == "ppred_examples_tokens")) {
  #   tokens_test <- test_data
  #   y_tokens_test <- tokens_test$token_y
  # }

  # output <- table(as.numeric(y_pred), y_tokens_test)
}
