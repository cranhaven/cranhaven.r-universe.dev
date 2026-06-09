#' Plot a confidence curve of attenuated correlation coefficients.
#'
#' @param x An object of class \code{ccaf}. The confidence curve to plot.
#' @param y Ignored; supported for compatibility with the \code{plot} generic.
#' @param level Level to highlight. Defaults to 0.95. If \code{NULL},
#'    highlights no level.
#' @param ... Passed to \code{plot}.
#' @return An invisible copy of \code{x}.
#' @export

plot.ccaf = function(x, y, level = 0.95, ...) {

  if (length(x) == 1) stop("plot requires more than one p-value.")

  if (attr(x, "type") == "Confidence curve") {
    yy = as.numeric(x)
  } else {
    yy = 1 - as.numeric(x)
  }

  xx = attr(x, "rho")
  r = attr(x, "r")

  old_mar = graphics::par()$mar
  on.exit(graphics::par(mar = old_mar))

  if (!is.null(level)) graphics::par(mar = old_mar + c(0, 0, 0, 1))

  value = min(abs(r[1] / (r[2] * r[3])), 1) * sign(r[1] / (r[2] * r[3]))

  supplied = list(...)
  defaults = list(type = "l",
                  col = "red3",
                  lwd = 2,
                  xlab = expression(rho),
                  ylab = "Confidence",
                  bty = "l")

  args = c(list(x = xx, y = yy),
           listmerge(x = defaults, y = supplied))

  do.call(graphics::plot.default, args)

  graphics::grid()
  graphics::abline(v = value, lty = 3, lwd = 1)
  do.call(graphics::lines.default, args)

  if (!is.null(level)) {
    graphics::abline(h = level, col = "orange")
    graphics::axis(side = 4, at = level, col = "orange", las = 2)
  }

  graphics::axis(side = 3, at = value,
                 labels = format(round(value, 2), nsmall = 1),
                 lty = 3, lwd = 1)

  invisible(x)
}

#' Add a plot a confidence curve of attenuated correlation coefficients.
#'
#' @param x An object of class \code{ccaf}. The confidence curve to plot.
#' @param type The type of plot.
#' @param col The color of the curve.
#' @param lwd The thickness of the curve.
#' @param ... Passed to \code{lines}.
#' @return An invisible copy of \code{x}.
#' @export
lines.ccaf = function(x, type = "l", col = "red3", lwd = 2, ...) {

  if (length(x) == 1) stop("plot requires more than one p-value.")

  if (attr(x, "type") == "Confidence curve") {
    yy = as.numeric(x)
  } else {
    yy = 1 - as.numeric(x)
  }

  graphics::lines.default(x = attr(x, "rho"),
                         y = yy,
                         type = type,
                         col = col,
                         lwd = lwd,
                         ...)

  invisible(x)

}
