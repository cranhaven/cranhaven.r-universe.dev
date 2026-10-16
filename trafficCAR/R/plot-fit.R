#' Plot observed vs predicted traffic values
#'
#' @param fit traffic_fit
#' @param data data.frame
#' @importFrom rlang .data
#' @return ggplot
#' @export
plot_observed_fitted <- function(fit, data) {
  if (!inherits(fit, "traffic_fit"))
    stop("`fit` must be a `traffic_fit`.")
  if (!is.data.frame(data))
    stop("`data` must be a data.frame.")
  if (is.null(fit$draws) || !is.list(fit$draws))
    stop("`fit$draws` must be a list.")
  if (is.null(fit$draws$mu))
    stop("`fit$draws$mu` is required.")

  mu <- fit$draws$mu

  # allow vector, matrix, or numeric data.frame
  if (is.data.frame(mu)) {
    if (!all(vapply(mu, is.numeric, logical(1))))
      stop("`fit$draws$mu` must be numeric.")
    mu <- as.matrix(mu)
  }

  if (is.null(dim(mu)))
    mu <- matrix(mu, nrow = 1)

  if (!is.matrix(mu) || !is.numeric(mu))
    stop("`fit$draws$mu` must be numeric.")


  if (is.null(fit$outcome_col) || !is.character(fit$outcome_col) ||
      length(fit$outcome_col) != 1L || !nzchar(fit$outcome_col)) {
    stop("`fit$outcome_col` must be a non-empty character scalar.")
  }
  if (!fit$outcome_col %in% names(data)) {
    stop("Required column `", fit$outcome_col, "` not found in `data`.")
  }

  if (is.null(fit$outcome_label) || !is.character(fit$outcome_label) ||
      length(fit$outcome_label) != 1L || !nzchar(fit$outcome_label)) {
    stop("`fit$outcome_label` must be a non-empty character scalar.")
  }

  pred <- colMeans(mu)
  obs <- data[[fit$outcome_col]]

  if (length(pred) != length(obs)) {
    stop(
      "Length mismatch: `fit$draws$mu` implies ", length(pred),
      " predictions but `data[[fit$outcome_col]]` has length ",
      length(obs), "."
    )
  }

  df <- data.frame(
    observed = obs,
    predicted = pred
  )

  ggplot2::ggplot(
    df,
    ggplot2::aes(x = .data$observed, y = .data$predicted)
  ) +
    ggplot2::geom_point(alpha = 0.6) +
    ggplot2::geom_abline(linetype = 2) +
    ggplot2::labs(
      x = paste("Observed", fit$outcome_label),
      y = paste("Predicted", fit$outcome_label)
    )
}

