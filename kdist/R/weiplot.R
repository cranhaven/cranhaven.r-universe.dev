#' Create Weibull Plot.
#'
#' A special type of plot where Weibull distributed data plots as a straight
#' line. This was also originally called Rayleigh paper. Both Rayleigh and
#' exponential distributions also plot as straight lines.
#'
#' @param data data values from which a cumulative density function will be
#'             estimated using \code{ecdf(data)}
#' @param n number of points required in plot (default n = 70).
#' @param type plot type
#' @param xlim the minimum and maximum to be used for the x-axis
#' @param ylim the minimum and maximum to be used for the y-axis
#' @param main the title of the plot
#' @param sub the sub-title of the plot
#' @param xlab the title of the x-axis
#' @param ylab the title of the left y-axis
#' @param ylab2 the title of the right y-axis
#' @param percent logical; display right hand axis as percentages 
#' @details A Weibull plot uses log paper and has log(1/(1-F(x)) versus x,
#'         where the data values x have an empirical cdf of F(x). The plot
#'         margins may need to be adjusted so that the right hand axis is
#'         visible.
#' 
#' @seealso \code{weilines()} adds lines to a Weibull plot
#' @examples
#'
#' graphics::par(mar = c(5, 5, 5, 5))
#' r <- rexp(100000)
#' weiplot(r, xlim = c(1e-3, 10))
#' x <- 10^seq(-3, 2, length = 100)
#' weilines(x, pexp(x))
#' @import graphics
#' @name weiplot
#' @export
# Create Weibull plot
weiplot <- function(data, n = 70, type = "p",  xlim = NULL, ylim = c(0.01, 10),
                  main = "Weibull Plot", sub = NULL,
                  ylab = "log(1/1-F(x))", ylab2 = "F(x)", xlab = "x",
                  percent = "False"){

  if (type == "n"){
    x = c(1, 1)
    F = c(0.5, 0.5)
  } else {
    fn <- stats::ecdf(data)

    if (is.null(xlim)){
       xlim = c(min(data[data > 0]), max(data))
    }

    x <- exp(seq(log(xlim[1]), log(xlim[2]), length = n))

    F <- fn(x)
  }

  plot(x, log(1 / (1 - F)), type = type, xlim = xlim,
       ylim = ylim, log = "xy", main = main, sub = sub,
       xlab = xlab, ylab = ylab)

  v = c(0.01, 0.1, 0.25, 0.5, 0.75, 0.99)

  tickv = log(1 / (1 - v))
 
  if (isTRUE(percent)){
     ticklab = c('1', '10', '25', '50', '75', '99')
  } else {
     ticklab = c('0.01', '0.1', '0.25', '0.5', '0.75', '0.99')
  }

  axis(4, at = tickv, labels = ticklab)

  cex = par()$cex
  
  mtext(ylab2, side = 4, line = 2, cex = cex)
}

#' Add Lines onto a Weibull Plot
#'
#' Weibull distributed data plots as a straight line on log-log plot using
#' \code{wlines()}. It is best used after function \code{wplot()} has been
#' called. 
#'
#' @param x vector of values
#' @param y vector of values the same length as x
#' @param lty line type
#' @param lwd line width
#' @param col line color
#' @param type type of plotting
#' @param pch symbol type for type = "b"
#' @details A Weibull plot uses log paper and has log(1/(1-F(x)) versus x,
#'         where the data values x have an empirical cdf of F(x). The plot
#'         margins may need to be adjusted so that the right hand axis is
#'         visible.
#' 
#' @seealso \code{wplot()} creates the Weibull plot
#' @examples
#'
#' dummy <- c(0,0)
#' weiplot(dummy, xlim = c(1e-3, 10), type = "n")
#' x <- 10^seq(-3, 2, length = 100)
#' weilines(x, pexp(x), col = "red")
#' weilines(x, pweibull(x, 2), col = "blue")
#' weilines(x, pweibull(x, 3), col = "green")
#' @name weilines
#' @export
# Put a line onto a Weibull plot
weilines <- function(x, y, lty = NULL, lwd = NULL, col = "black",
                   type = "l", pch = 0){

  lines(x, log(1 / (1 - y)), lty = lty, lwd = lwd, col = col,
                            type = type, pch = pch)
}
