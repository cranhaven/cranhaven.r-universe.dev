# ==============================================================================
# ggplot2 autoplot methods for matching results and diagnostics
# ==============================================================================

#' ggplot2 autoplot for matching results
#'
#' Produces ggplot2-based visualizations of matching distance distributions.
#' Returns a ggplot object that can be further customized.
#'
#' @param object A matching_result object
#' @param type Type of plot: "histogram" (default), "density", or "ecdf"
#' @param ... Additional arguments (ignored)
#'
#' @return A ggplot object
#'
#' @details
#' Use `plot()` for base graphics or `autoplot()` for ggplot2 output.
#' The ggplot2 package must be installed.
#'
#' @examples
#' \donttest{
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   left <- data.frame(id = 1:5, x = c(1, 2, 3, 4, 5))
#'   right <- data.frame(id = 6:10, x = c(1.1, 2.2, 3.1, 4.2, 5.1))
#'   result <- match_couples(left, right, vars = "x")
#'   ggplot2::autoplot(result)
#'   ggplot2::autoplot(result, type = "density")
#' }
#' }
#'
#' @exportS3Method ggplot2::autoplot
autoplot.matching_result <- function(object,
                                     type = c("histogram", "density", "ecdf"),
                                     ...) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 required for autoplot(). Use plot() for base graphics.",
         call. = FALSE)
  }

  type <- match.arg(type)
  distances <- object$pairs$distance

  if (length(distances) == 0) {
    message("No matched pairs to plot")
    return(invisible(NULL))
  }

  df <- data.frame(distance = distances)
  title <- paste0("Matching Distances (n=", length(distances), ")")

  p <- switch(type,
    histogram = {
      ggplot2::ggplot(df, ggplot2::aes(x = .data$distance)) +
        ggplot2::geom_histogram(
          fill = "steelblue", color = "white", bins = 30
        ) +
        ggplot2::labs(title = title, x = "Distance", y = "Count")
    },
    density = {
      ggplot2::ggplot(df, ggplot2::aes(x = .data$distance)) +
        ggplot2::geom_density(fill = "steelblue", alpha = 0.6) +
        ggplot2::labs(title = title, x = "Distance", y = "Density")
    },
    ecdf = {
      ggplot2::ggplot(df, ggplot2::aes(x = .data$distance)) +
        ggplot2::stat_ecdf(color = "steelblue", linewidth = 1) +
        ggplot2::labs(title = title, x = "Distance",
                      y = "Cumulative Proportion")
    }
  )

  p + .theme_couplr()
}

#' ggplot2 autoplot for balance diagnostics
#'
#' Produces ggplot2-based balance assessment plots. Returns a ggplot object.
#'
#' @param object A balance_diagnostics object
#' @param type Type of plot: "love" (default), "histogram", or "variance"
#' @param threshold Threshold for standardized differences (default: 0.1)
#' @param ... Additional arguments (ignored)
#'
#' @return A ggplot object
#'
#' @examples
#' \donttest{
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   set.seed(42)
#'   left <- data.frame(id = 1:10, age = rnorm(10, 45, 10),
#'                      income = rnorm(10, 50000, 15000))
#'   right <- data.frame(id = 11:30, age = rnorm(20, 47, 10),
#'                        income = rnorm(20, 52000, 15000))
#'   result <- match_couples(left, right, vars = c("age", "income"))
#'   bal <- balance_diagnostics(result, left, right, vars = c("age", "income"))
#'   ggplot2::autoplot(bal)
#' }
#' }
#'
#' @exportS3Method ggplot2::autoplot
autoplot.balance_diagnostics <- function(object,
                                          type = c("love", "histogram",
                                                   "variance"),
                                          threshold = 0.1,
                                          ...) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 required for autoplot(). Use plot() for base graphics.",
         call. = FALSE)
  }

  type <- match.arg(type)

  switch(type,
    love = .autoplot_love(object, threshold),
    histogram = .autoplot_hist(object, threshold),
    variance = .autoplot_variance(object)
  )
}

#' Love plot via ggplot2
#' @keywords internal
.autoplot_love <- function(object, threshold = 0.1) {
  df <- data.frame(
    variable = object$var_stats$variable,
    std_diff = object$var_stats$std_diff,
    stringsAsFactors = FALSE
  )
  df$abs_std_diff <- abs(df$std_diff)
  df$variable <- factor(df$variable,
                         levels = df$variable[order(df$abs_std_diff)])
  df$balanced <- df$abs_std_diff <= threshold

  ggplot2::ggplot(df, ggplot2::aes(
    x = .data$std_diff, y = .data$variable, color = .data$balanced
  )) +
    ggplot2::geom_point(size = 3) +
    ggplot2::geom_vline(xintercept = 0, linetype = "solid", color = "gray50") +
    ggplot2::geom_vline(xintercept = c(-threshold, threshold),
                        linetype = "dashed", color = "firebrick") +
    ggplot2::scale_color_manual(
      values = c("TRUE" = "steelblue", "FALSE" = "firebrick"),
      labels = c("TRUE" = "Balanced", "FALSE" = "Imbalanced"),
      name = NULL
    ) +
    ggplot2::labs(
      title = "Balance: Standardized Differences",
      x = "Standardized Difference", y = NULL
    ) +
    .theme_couplr() +
    ggplot2::theme(legend.position = "bottom")
}

#' Histogram of |std diff| via ggplot2
#' @keywords internal
.autoplot_hist <- function(object, threshold = 0.1) {
  df <- data.frame(abs_std_diff = abs(object$var_stats$std_diff))

  ggplot2::ggplot(df, ggplot2::aes(x = .data$abs_std_diff)) +
    ggplot2::geom_histogram(fill = "steelblue", color = "white", bins = 15) +
    ggplot2::geom_vline(xintercept = threshold, linetype = "dashed",
                        color = "firebrick", linewidth = 1) +
    ggplot2::labs(
      title = "Distribution of |Standardized Differences|",
      x = "|Standardized Difference|", y = "Count"
    ) +
    .theme_couplr()
}

#' Variance ratio plot via ggplot2
#' @keywords internal
.autoplot_variance <- function(object) {
  df <- data.frame(
    variable = object$var_stats$variable,
    var_ratio = object$var_stats$var_ratio,
    stringsAsFactors = FALSE
  )
  df$log_ratio <- abs(log(df$var_ratio))
  df$variable <- factor(df$variable,
                         levels = df$variable[order(df$log_ratio)])
  df$balanced <- df$var_ratio >= 0.5 & df$var_ratio <= 2

  ggplot2::ggplot(df, ggplot2::aes(
    x = .data$var_ratio, y = .data$variable, color = .data$balanced
  )) +
    ggplot2::geom_point(size = 3) +
    ggplot2::geom_vline(xintercept = 1, linetype = "solid", color = "gray50") +
    ggplot2::geom_vline(xintercept = c(0.5, 2), linetype = "dashed",
                        color = "firebrick") +
    ggplot2::scale_x_log10() +
    ggplot2::scale_color_manual(
      values = c("TRUE" = "steelblue", "FALSE" = "firebrick"),
      labels = c("TRUE" = "Balanced", "FALSE" = "Imbalanced"),
      name = NULL
    ) +
    ggplot2::labs(
      title = "Balance: Variance Ratios",
      x = "Variance Ratio (left/right, log scale)", y = NULL
    ) +
    .theme_couplr() +
    ggplot2::theme(legend.position = "bottom")
}

#' ggplot2 autoplot for sensitivity analysis
#'
#' Plots p-value upper bounds against sensitivity parameter Gamma.
#'
#' @param object A sensitivity_analysis object
#' @param alpha Significance level (default: 0.05)
#' @param ... Additional arguments (ignored)
#'
#' @return A ggplot object
#'
#' @exportS3Method ggplot2::autoplot
autoplot.sensitivity_analysis <- function(object, alpha = 0.05, ...) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 required for autoplot(). Use plot() for base graphics.",
         call. = FALSE)
  }

  df <- object$results

  ggplot2::ggplot(df, ggplot2::aes(x = .data$gamma, y = .data$p_upper)) +
    ggplot2::geom_line(color = "steelblue", linewidth = 1) +
    ggplot2::geom_point(color = "steelblue", size = 2) +
    ggplot2::geom_hline(yintercept = alpha, linetype = "dashed",
                        color = "firebrick") +
    ggplot2::labs(
      title = "Rosenbaum Sensitivity Analysis",
      x = expression(Gamma ~ "(sensitivity parameter)"),
      y = "Upper bound p-value"
    ) +
    .theme_couplr()
}
