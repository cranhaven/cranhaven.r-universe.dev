#' Plot a \code{mmc} Object
#'
#' The \code{plot()} method for objects of the
#' class \code{mmc} gives a plot of the best and current p-value found during the iterations of \code{mmc}.
#'
#' @return The \code{mmc} object is returned invisibly.
#'
#' @param x An object of class \code{mmc}.
#' @param ... Arguments to be passed to methods, such as \link{graphical parameters} (see \code{\link{par}}).
#' @export
#'
#' @example /inst/examples/plot_mmc_example.R
#'
plot.mmc <- function(x, ...) {
    # Extract information from mmc object
    opt_trace <- x$opt_trace
    alpha <- x$alpha

    # Plot iterations on graph
    graphics::plot(opt_trace$pval, xlab="Iterations",
         ylab="P-value", main = "Evolution of mmc", ...)
    graphics::lines(opt_trace$max,col="green")

    # Add line if level is specified
    if(!is.null(alpha)){
        lapply(alpha, function(alpha){graphics::abline(alpha,0,lty=2, col="red")})
    }

    invisible(x)
}
