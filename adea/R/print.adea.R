#' Print method for adea class
#' @name print.adea
#' @param x Object of class adea to print.
#' @param ... Optional arguments to "print".
#' @method print adea
#' @export
print.adea <- function(x, ...) {
    print(x$eff, ...)
    invisible(x)
}
