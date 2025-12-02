
#' Reliability Growth Analysis.
#'
#' This function performs reliability growth analysis using the Crow-AMSAA model by
#' Crow (1975) <https://apps.dtic.mil/sti/citations/ADA020296> or piecewise
#' NHPP model by Guo et al. (2010) <doi:10.1109/RAMS.2010.5448029>.
#'
#' @param times A vector of cumulative failure times.
#' @param failures A vector of the number of failures at each corresponding time in times.
#' @param model_type The model type. Either `Crow-AMSAA` (default) or `Piecewise NHPP` with change point detection.
#' @param breaks An optional vector of breakpoints for the `Piecewise NHPP` model.
#' @param conf_level The desired confidence level, which defaults to 95%.
#' @return The function returns an object of class `rga` that contains:
#' \item{model}{The fitted model object (lm or segmented).}
#' \item{logLik}{The log-likelihood of the fitted model.}
#' \item{AIC}{Akaike Information Criterion.}
#' \item{BIC}{Bayesian Information Criterion.}
#' \item{breakpoints}{Breakpoints (log scale) if applicable.}
#' \item{fitted_values}{Fitted cumulative failures on the original scale.}
#' \item{lower_bounds}{Lower confidence bounds (original scale).}
#' \item{upper_bounds}{Upper confidence bounds (original scale).}
#' \item{betas}{Estimated beta(s).}
#' \item{lambdas}{Estimated lambda(s).}
#'
#' @examples
#' times <- c(100, 200, 300, 400, 500)
#' failures <- c(1, 2, 1, 3, 2)
#' result <- rga(times, failures)
#' print(result)
#'
#' @importFrom stats lm predict AIC BIC logLik
#' @importFrom segmented segmented slope intercept seg.control
rga <- function(times, failures, model_type = "Crow-AMSAA", breaks = NULL, conf_level = 0.95) {

  # Validation checks
  if (length(times) != length(failures)) {
    stop("The length of 'times' and 'failures' must be equal.")
  }
  if (any(times <= 0)) stop("All values in 'times' must be greater than 0.")
  if (any(failures <= 0)) stop("All values in 'failures' must be greater than 0.")
  if (!is.numeric(conf_level) || conf_level <= 0 || conf_level >= 1) {
    stop("Conf_level must be a numeric value between 0 and 1 (exclusive).")
  }

  valid_models <- c("Crow-AMSAA", "Piecewise NHPP")
  if (!(model_type %in% valid_models)) {
    stop(paste("Model_type must be one of", paste(valid_models, collapse = ", "), "."))
  }

  if (!is.null(breaks)) {
    if (!is.numeric(breaks) || any(breaks <= 0)) {
      stop("Breakpoints must be a numeric vector with positive values.")
    }
    if (model_type != "Piecewise NHPP") {
      stop("Breakpoints can only be used with the 'Piecewise NHPP' model.")
    }
  }

  # Data prep
  cum_failures <- cumsum(failures)
  cum_time <- cumsum(times)
  log_times <- log(cum_time)
  log_cum_failures <- log(cum_failures)

  fit <- stats::lm(log_cum_failures ~ log_times)

  if (model_type == "Piecewise NHPP") {
    if (is.null(breaks)) {
      updated_fit <- segmented::segmented(fit, seg.Z = ~log_times)
      breakpoints <- updated_fit$psi[, "Est."]
    } else {
      breakpoints <- log(breaks)
      updated_fit <- segmented::segmented(fit, seg.Z = ~log_times, psi = breakpoints)
    }
    slopes <- segmented::slope(updated_fit)
    intercepts <- segmented::intercept(updated_fit)
    betas <- slopes
    lambdas <- exp(intercepts$log_times)

    # Standard errors
    beta_se <- slopes$log_times[, "St.Err."]

  } else {
    updated_fit <- fit
    breakpoints <- NULL
    smry <- summary(updated_fit)
    slope <- smry$coefficients[2, ]
    intercept <- smry$coefficients[1, ]
    betas <- slope["Estimate"]
    lambdas <- exp(intercept["Estimate"])

    # Standard Error
    beta_se <- slope["Std. Error"]
  }

  # Fit statistics
  loglik <- as.numeric(stats::logLik(updated_fit))
  aic <- stats::AIC(updated_fit)
  bic <- stats::BIC(updated_fit)

  # Predictions values
  fitted_values <- stats::predict(updated_fit)
  conf_intervals <- stats::predict(updated_fit, interval = "confidence", level = conf_level)
  lower_bounds <- exp(conf_intervals[, "lwr"])
  upper_bounds <- exp(conf_intervals[, "upr"])

  # Return object
  result <- list(
    log_times = log_times,
    log_cum_failures = log_cum_failures,
    logLik = loglik,
    AIC = aic,
    BIC = bic,
    breakpoints = breakpoints,
    fitted_values = exp(fitted_values),
    lower_bounds = lower_bounds,
    upper_bounds = upper_bounds,
    betas = betas,
    betas_se = beta_se,
    lambdas = lambdas
  )
  return(result)
}

#' Plot Method for RGA Objects
#'
#' This function generates plots for objects of class \code{rga}.
#'
#' @param x A list of results from the \code{rga} function.
#' @param conf_bounds Logical; include confidence bounds (default: TRUE).
#' @param legend Logical; show the legend (default: TRUE).
#' @param log Logical; use a log-log scale (default: FALSE).
#' @param legend_pos Position of the legend (default: "bottomright").
#' @param ... Additional arguments passed to \code{plot()}.
#' @return Invisibly returns \code{NULL}.
#' @examples
#' times <- c(100, 200, 300, 400, 500)
#' failures <- c(1, 2, 1, 3, 2)
#' result <- rga(times, failures)
#' plot(result, main = "Reliability Growth Analysis",
#' xlab = "Cumulative Time", ylab = "Cumulative Failures")
#' @importFrom graphics lines abline legend plot
plot.rga <- function(x,
                     conf_bounds = TRUE,
                     legend = TRUE,
                     log = FALSE,
                     legend_pos = "bottomright",
                     ...) {
  if (!is.logical(conf_bounds) || !is.logical(legend) || !is.logical(log)) {
    stop("Arguments 'conf_bounds', 'legend', and 'log' must be logical.")
  }

  times <- exp(x$log_times)
  cum_failures <- exp(x$log_cum_failures)

  # Base plot
  plot_args <- list(
    x = times,
    y = cum_failures,
    pch = 16,
    ...
  )
  if (log) plot_args$log <- "xy"
  do.call(graphics::plot, plot_args)

  # Fitted line
  graphics::lines(times, x$fitted_values, ...)

  # Confidence bounds
  if (conf_bounds) {
    graphics::lines(times, x$lower_bounds, lty = 2, ...)
    graphics::lines(times, x$upper_bounds, lty = 2, ...)
  }

  # Breakpoints
  if (!is.null(x$breakpoints)) {
    graphics::abline(v = exp(x$breakpoints), lty = 3)
  }

  # Legend
  if (legend) {
    legend_items <- list(
      labels = c("Observed", "Fitted Line"),
      cols = c("black", "black"),
      pch = c(16, NA),
      lty = c(NA, 1)
    )

    if (conf_bounds) {
      legend_items$labels <- c(legend_items$labels, "Confidence Bounds")
      legend_items$cols <- c(legend_items$cols, "black")
      legend_items$pch <- c(legend_items$pch, NA)
      legend_items$lty <- c(legend_items$lty, 2)
    }

    if (!is.null(x$breakpoints)) {
      legend_items$labels <- c(legend_items$labels, "Change Points")
      legend_items$cols <- c(legend_items$cols, "black")
      legend_items$pch <- c(legend_items$pch, NA)
      legend_items$lty <- c(legend_items$lty, 3)
    }

    graphics::legend(
      legend_pos,
      legend = legend_items$labels,
      col = legend_items$cols,
      pch = legend_items$pch,
      lty = legend_items$lty,
      bty = "n"
    )
  }

  invisible(NULL)
}

