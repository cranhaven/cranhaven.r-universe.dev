#' Print method for cadea class
#' @name print.cadea
#' @param x Object of class cadea to print.
#' @param ... Optional arguments to "print".
#' @method print cadea
#' @export
print.cadea <- function(x, ...) {
    eff <- x$eff
    print(eff, ...)
    invisible(x)
}
