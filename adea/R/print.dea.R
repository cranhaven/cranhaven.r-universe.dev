#' Print method for dea class
#' @name print.dea
#' @param x Object of class dea to print.
#' @param ... Optional arguments to "print".
#' @method print dea
#' @export
print.dea <- function(x, ...) {
    print(x$eff, ...)
    invisible(x)
}
