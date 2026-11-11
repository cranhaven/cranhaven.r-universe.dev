#' @title Plot Methods
#'
#' @description  Visualize metric
#' @param x Data to plot. An object of type [`ppred_predictions`].
#' @param ... Additional variables
#' @return A ggplot object, which can be customized further, if deemed necessary.
#'
#' @concept visualization
#'
#' @export
plot.ppred_predictions <- function(x, ...) {
  task <- predictions %>% attr("task")
  y_var <- predictions %>% attr("y_var")

  # outcome & next_activity
  if (task %in% c("outcome", "next_activity")) {

    # plot confusion matrix with geom_tile
    predictions <- as.data.frame(table(predictions[[y_var]],
                                       predictions[[paste0("pred_", task)]]))

    Var1 <- NULL
    Var2 <- NULL
    Freq <- NULL
    ggplot(predictions, aes(as.factor(Var1), as.factor(Var2), fill = Freq)) +
      geom_tile() +
      geom_text(aes(label = Freq)) +
      scale_fill_gradient(low="white", high="#009194") +
      labs(x = "Reference",y = "Prediction")
  }

  # next time & remaining time
  else if(task %in% c("next_time", "remaining_time")) {

    predictions %>% ggplot(aes(get(paste0("actual_start_", task)),
                        get(paste0("pred_start_", task)))) +
      geom_point(aes(color = get(activity_id(predictions)))) +
      labs(x = "Reference", y = "Prediction") +
      theme(legend.title = element_blank()) -> p
      plotly::ggplotly(p)
  }

  # remaining trace
  else {
    cli::cli_abort("Does not support remaining_trace task")
  }
}
