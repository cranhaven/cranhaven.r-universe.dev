#' Plot the cross-validation curve from a class `cvobj` object
#'
#' Plots the cross-validation curve, and upper and lower standard deviation
#' curves, as a function of the `lambda` values used.
#'
#' @param x A `"cvobj"` object.
#' @param sign.lambda Either plot against `log(lambda)` (default) or its
#' negative if `sign.lambda = -1`.
#' @param log.lambda If `TRUE` (default), x-axis is `log(lambda)` instead of
#' `lambda` (`log.lambda = FALSE`).
#' @param ... Other graphical parameters to plot.
#'
#' @return A plot is produced, and nothing is returned.
#'
#' @importFrom graphics abline points
#' @export
plot.cvobj <- function(x, sign.lambda = 1, log.lambda = TRUE, ...) {
  cvobj <- x

  if (log.lambda) {
    xval <- log(cvobj$lambda)
    lambda.min <- log(cvobj$lambda.min)
    lambda.1se <- log(cvobj$lambda.1se)
    xlab <- "log(Lambda)"
  } else {
    xval <- cvobj$lambda
    lambda.min <- cvobj$lambda.min
    lambda.1se <- cvobj$lambda.1se
    xlab <- "Lambda"
  }

  if (sign.lambda < 0) {
    xval <- -1 * xval
    lambda.min <- -1 * lambda.min
    lambda.1se <- -1 * lambda.1se
    xlab <- paste0("-", xlab)
  }
  plot.args <- list(x = xval, y = cvobj$cvm,
                    ylim = range(cvobj$cvup, cvobj$cvlo),
                    xlab = xlab, ylab = cvobj$name, type = "n")
  new.args <- list(...)
  if (length(new.args)) plot.args[names(new.args)] <- new.args

  do.call("plot", plot.args)
  error.bars(xval, cvobj$cvup, cvobj$cvlo,
             width = 0.01, col="darkgrey")
  points(xval, cvobj$cvm, pch = 20,
         col = "red")
  abline(v = lambda.min, lty = 3)
  abline(v = lambda.1se, lty = 3)
  invisible()
}

#' @importFrom graphics segments
error.bars <- function(x, upper, lower, width = 0.02, ...) {
  xlim <- range(x)
  barw <- diff(xlim) * width
  segments(x, upper, x, lower, ...)
  segments(x - barw, upper, x + barw, upper, ...)
  segments(x - barw, lower, x + barw, lower, ...)
  range(upper, lower)
}
