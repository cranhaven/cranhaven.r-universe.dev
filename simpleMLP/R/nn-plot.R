#' Plot Accuracy
#'
#' Plot the training and validation accuracy.
#'
#' @param accuracy_train list of training accuracy
#' @param accuracy_validate list of validation accuracy
#'
#' @import ggplot2
plot_accuracy <- function(accuracy_train, accuracy_validate) {
  train_df <- stats::na.omit(data.frame(
    c(1:length(accuracy_train)),
    accuracy_train,
    rep("Train", length(accuracy_train))
  ))
  colnames(train_df) <- c("n", "accuracy", "set")

  validate_df <- stats::na.omit(data.frame(
    c(1:length(accuracy_validate)),
    accuracy_validate,
    rep("Validate", length(accuracy_validate))
  ))
  colnames(validate_df) <- c("n", "accuracy", "set")

  df <- base::rbind(train_df, validate_df)

  accuracy_plot <- ggplot(data = df, aes_string(x = "n", y = "accuracy", color = "set")) +
    geom_point() +
    geom_line() +
    labs(
      x = "Iterations",
      y = "Accuracy"
    ) +
    scale_y_continuous(limits = c(0, 1)) +
    theme_minimal() +
    theme(
      legend.position = c(0.8, 0.2),
      legend.title = element_blank()
    ) +
    scale_color_manual(values = c("Train" = "#FFAE00", "Validate" = "#0077FF"))

  suppressWarnings(print(accuracy_plot))
}
