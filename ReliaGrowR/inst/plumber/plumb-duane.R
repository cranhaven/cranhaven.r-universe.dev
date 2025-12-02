
#' Duane Analysis
#'
#' This function performs a Duane analysis (1962) <doi:10.1109/TA.1964.4319640>
#' on failure data by fitting a log-log linear regression of cumulative MTBF
#' versus cumulative time.
#'
#' @param times A numeric vector of cumulative failure times.
#' @param failures A numeric vector of the number of failures at each corresponding time in \code{times}.
#' @param conf.int Logical; whether to compute confidence bounds (default: \code{FALSE}).
#' @param conf.level Confidence level for the confidence bounds (default: \code{0.95}).
#'
#' @return A list of class \code{"duane"} containing:
#' \item{model}{The fitted \code{lm} object.}
#' \item{logLik}{The log-likelihood of the fitted model.}
#' \item{AIC}{Akaike Information Criterion.}
#' \item{BIC}{Bayesian Information Criterion.}
#' \item{conf.level}{The confidence level, if applicable.}
#' \item{Cumulative_Time}{The cumulative operating times.}
#' \item{Cumulative_MTBF}{The cumulative mean time between failures.}
#' \item{Fitted_Values}{The fitted values on the MTBF scale.}
#' \item{Confidence_Bounds}{Matrix of fitted values and confidence bounds on the MTBF scale (if applicable).}
#'
#' @examples
#' times <- c(100, 200, 300, 400, 500)
#' failures <- c(1, 2, 1, 3, 2)
#' fit <- duane(times, failures, conf.int = TRUE, conf.level = 0.90)
#' print(fit)
#'
#' @importFrom stats lm AIC BIC logLik predict
duane <- function(times, failures, conf.int = FALSE, conf.level = 0.95) {
  if (length(times) != length(failures)) {
    stop("The length of 'times' and 'failures' must be equal.")
  }
  if (any(times <= 0)) stop("All values in 'times' must be > 0.")
  if (any(failures <= 0)) stop("All values in 'failures' must be > 0.")

  # Cumulative times & failures
  cum_failures <- cumsum(failures)
  cum_times <- cumsum(times)

  # cumulative MTBF
  cum_mtbf <- cum_times / cum_failures

  # linear model (log-log)
  log_cum_times <- log(cum_times)
  log_cum_mtbf <- log(cum_mtbf)
  fit <- stats::lm(log_cum_mtbf ~ log_cum_times)

  # fit stats
  aic <- stats::AIC(fit)
  bic <- stats::BIC(fit)
  loglik <- as.numeric(stats::logLik(fit))

  # fitted values back-transformed
  fitted_values <- exp(predict(fit))

  # CI if requested
  ci_bounds <- NULL
  used_conf_level <- NULL
  if (conf.int) {
    pred <- predict(fit, interval = "confidence", level = conf.level)
    ci_bounds <- exp(pred)
    used_conf_level <- conf.level
  }

  result <- list(
    logLik = loglik,
    AIC = aic,
    BIC = bic,
    conf.level = used_conf_level,
    Cumulative_Time = cum_times,
    Cumulative_MTBF = cum_mtbf,
    Fitted_Values = fitted_values,
    Confidence_Bounds = ci_bounds
  )
  return(result)
}

#' Plot Method for Duane Analysis
#'
#' Generates a Duane plot (log-log or linear scale) with fitted regression line
#' and optional confidence bounds.
#'
#' @param x A list of results from the \code{duane} function.
#' @param log Logical; whether to use logarithmic scales for axes (default: \code{TRUE}).
#' @param point_col Color for the observed data points (default: "black").
#' @param line_col Color for the fitted line (default: "black").
#' @param conf_col Color for the confidence bound lines (default: "red").
#' @param legend Logical; whether to include a legend (default: TRUE).
#' @param legend_pos Position of the legend (default: "bottomright").
#' @param ... Further arguments passed to \code{plot()}.
#'
#' @return Invisibly returns \code{NULL}.
#' @examples
#' times <- c(100, 200, 300, 400, 500)
#' failures <- c(1, 2, 1, 3, 2)
#' fit <- duane(times, failures, conf.int = TRUE)
#' plot(fit, main = "Duane Plot", xlab = "Cumulative Time", ylab = "Cumulative MTBF")
#' @importFrom graphics legend lines plot
plot.duane <- function(x,
                       log = TRUE,
                       point_col = "black", line_col = "black",
                       conf_col = "black",
                       legend = TRUE,
                       legend_pos = "bottomright",
                       ...) {

  cum_time <- x$Cumulative_Time
  cum_mtbf <- x$Cumulative_MTBF

  plot_args <- list(
    x = cum_time,
    y = cum_mtbf,
    pch = 16,
    col = point_col,
    ...
  )
  if (log) plot_args$log <- "xy"
  do.call(graphics::plot, plot_args)

  # fitted line
  graphics::lines(cum_time, x$Fitted_Values, col = line_col, lty = 1)

  # confidence bounds
  if (!is.null(x$Confidence_Bounds)) {
    graphics::lines(cum_time, x$Confidence_Bounds[, "lwr"], col = conf_col, lty = 2)
    graphics::lines(cum_time, x$Confidence_Bounds[, "upr"], col = conf_col, lty = 2)
  }

  # legend
  if (legend) {
    legend_items <- c("Observed", "Fitted Line")
    cols <- c(point_col, line_col)
    pch <- c(16, NA)
    lty <- c(NA, 1)

    if (!is.null(x$Confidence_Bounds)) {
      legend_items <- c(legend_items, "Confidence Bounds")
      cols <- c(cols, conf_col)
      pch <- c(pch, NA)
      lty <- c(lty, 2)
    }

    graphics::legend(legend_pos, legend = legend_items,
                     col = cols, pch = pch, lty = lty, bty = "n")
  }

  invisible(NULL)
}
