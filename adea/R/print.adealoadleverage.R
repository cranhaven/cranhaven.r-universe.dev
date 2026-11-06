#' Print method for adealoadleverage class
#'
#' Print adealoadleverage results in human readable way
#' 
#' @name print.adealoadleverage
#' @param x Object of class adealoadleverage to print.
#' @param ... Optional arguments to "print".
#' @method print adealoadleverage
#' @export
print.adealoadleverage <- function(x, ...) {
    DMUs <- x$dmu.indexs
    if (!is.null(ncol(DMUs))) DMUs <- apply(DMUs, 1, FUN = function(x) paste(x[!is.na(x)], collapse = ", "))
    s <- data.frame(load = x$loads, load.diff = x$loads.diff, DMUs = DMUs)
    print(s, ...)
    invisible(x)
}
