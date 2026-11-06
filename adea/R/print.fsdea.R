#' Print method for fsdea class
#' @name print.fsdea
#' @param x Object of class fsdea to print.
#' @param ... Optional arguments to "print".
#' @method print fsdea
#' @export
print.fsdea <- function(x, ...) {
    print(x$eff, ...)
    cat(gettext('Selected inputs '), ': ', paste(x$inputnames[x$iselected == 1], collapse = ', '), '\n', sep = '')
    cat(gettext('Selected outputs'), ': ', paste(x$outputnames[x$oselected == 1], collapse = ', '), '\n', sep = '')
    invisible(x)
}
