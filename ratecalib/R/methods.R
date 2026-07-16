#' Extract calibration diagnostics
#'
#' @param x A `pass_rate_calibration` object.
#' @param sort_targets Whether to sort targets by absolute error, descending.
#' @return A list with target, margin and weight diagnostics.
#' @examples
#' d <- example_rate_data(300)
#' fit <- calibrate_rates(d, "qualified", "initial_weight",
#'                        groups = list(sex = c(M = 0.72, F = 0.68)))
#' calibration_diagnostics(fit)
#' @export
calibration_diagnostics <- function(x, sort_targets = TRUE) {
  if (!inherits(x, "pass_rate_calibration")) {
    stop("x must be a pass_rate_calibration object.", call. = FALSE)
  }
  target_check <- x$target_check
  if (isTRUE(sort_targets)) {
    target_check <- target_check[order(-target_check$abs_error), , drop = FALSE]
    rownames(target_check) <- NULL
  }
  list(targets = target_check, margins = x$margin_check, weights = x$diagnostics)
}

#' Methods for pass_rate_calibration objects
#'
#' S3 methods that print, summarize and plot calibration results.
#'
#' @param x,object A `pass_rate_calibration` object (or, for the summary
#'   printer, a `summary_pass_rate_calibration` object).
#' @param digits Number of decimal places to display.
#' @param top Number of largest-error targets to display.
#' @param type Plot type: `"target_error"` for target errors or
#'   `"multipliers"` for the weight-multiplier histogram.
#' @param row.names,optional Standard [as.data.frame()] arguments (ignored).
#' @param ... Further arguments passed to the underlying print or graphics
#'   functions.
#' @return `print` and `plot` return their input invisibly; `summary` returns a
#'   `summary_pass_rate_calibration` object; `weights` returns the calibrated
#'   weight vector; `as.data.frame` returns the data with the calibrated weight
#'   column.
#' @examples
#' d <- example_rate_data(300)
#' fit <- calibrate_rates(d, "qualified", "initial_weight",
#'                        groups = list(sex = c(M = 0.72, F = 0.68)))
#' print(fit)
#' summary(fit)
#' head(weights(fit))
#' head(as.data.frame(fit))
#' plot(fit, type = "target_error")
#' @name pass_rate_calibration-methods
NULL

#' @rdname pass_rate_calibration-methods
#' @importFrom stats weights
#' @export
weights.pass_rate_calibration <- function(object, ...) {
  object$data[[object$settings$new_weight]]
}

#' @rdname pass_rate_calibration-methods
#' @export
as.data.frame.pass_rate_calibration <- function(x, row.names = NULL,
                                                 optional = FALSE, ...) {
  as.data.frame(x$data, row.names = row.names, optional = optional, ...)
}

#' @rdname pass_rate_calibration-methods
#' @export
print.pass_rate_calibration <- function(x, digits = 4, ...) {
  d <- stats::setNames(x$diagnostics$value, x$diagnostics$metric)
  cat("Pass-rate weight calibration result\n")
  cat("  Mode:               ", if (x$settings$mode == "soft") "soft" else "exact", "\n", sep = "")
  cat("  Solver status:      ", x$solver_status, "\n", sep = "")
  cat("  Sample size:        ", format(d[["sample_size"]], scientific = FALSE), "\n", sep = "")
  cat("  Optimization cells: ", format(d[["observed_optimization_cells"]], scientific = FALSE), "\n", sep = "")
  cat("  Max target error:   ", format(round(d[["maximum_absolute_target_error"]], digits), nsmall = digits), "\n", sep = "")
  cat("  Multiplier range:   ", format(round(d[["minimum_multiplier"]], digits), nsmall = digits),
      " to ", format(round(d[["maximum_multiplier"]], digits), nsmall = digits), "\n", sep = "")
  cat("  Calibrated ESS:     ", format(round(d[["calibrated_ESS"]], 1), scientific = FALSE), "\n", sep = "")
  invisible(x)
}

#' @rdname pass_rate_calibration-methods
#' @export
summary.pass_rate_calibration <- function(object, top = 10L, ...) {
  top <- max(1L, as.integer(top))
  target_check <- object$target_check[order(-object$target_check$abs_error), , drop = FALSE]
  structure(list(call = object$call, settings = object$settings,
                 solver_status = object$solver_status,
                 diagnostics = object$diagnostics,
                 largest_target_errors = utils::head(target_check, top)),
            class = "summary_pass_rate_calibration")
}

#' @rdname pass_rate_calibration-methods
#' @export
print.summary_pass_rate_calibration <- function(x, digits = 4, ...) {
  cat("Pass-rate weight calibration summary\n\nCall:\n")
  print(x$call)
  cat("\nSolver status: ", x$solver_status, "\n", sep = "")
  cat("Mode: ", if (x$settings$mode == "soft") "soft" else "exact", "\n\n", sep = "")
  d <- x$diagnostics
  d$value <- round(d$value, digits)
  cat("Weight diagnostics:\n")
  print(d[c("metric", "value")], row.names = FALSE)
  cat("\nTargets with the largest errors:\n")
  shown <- x$largest_target_errors
  numeric_cols <- vapply(shown, is.numeric, logical(1))
  shown[numeric_cols] <- lapply(shown[numeric_cols], round, digits = digits)
  print(shown, row.names = FALSE)
  invisible(x)
}

#' @rdname pass_rate_calibration-methods
#' @export
plot.pass_rate_calibration <- function(x, type = c("target_error", "multipliers"), top = 20L, ...) {
  type <- match.arg(type)
  if (type == "target_error") {
    d <- x$target_check
    d$label <- paste(d$variable, d$level, sep = ": ")
    d <- utils::head(d[order(d$abs_error, decreasing = TRUE), , drop = FALSE], max(1L, as.integer(top)))
    graphics::barplot(rev(d$error), names.arg = rev(d$label), horiz = TRUE, las = 1,
                      xlab = "calibrated pass rate - target pass rate", main = "Target pass-rate error", ...)
    graphics::abline(v = 0, lty = 2)
  } else {
    graphics::hist(x$cell_weights$.multiplier, xlab = "calibrated weight / initial weight",
                   main = "Distribution of weight-adjustment multipliers", ...)
  }
  invisible(x)
}
