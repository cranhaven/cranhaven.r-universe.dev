#' Print method for adeastepwise class
#'
#' For the final model of adeastepwise function prints a summary, the input variables, and outputs variables.
#' 
#' @name print.adeastepwise
#' @param x Object of class stepwise to print.
#' @param ... Optional arguments to "print".
#' @method print adeastepwise
print.adeastepwise <- function(x, ...) {
    models <- summary(x)
    models <- models$models
    print(setNames(models, gettext(names(models))), ...)
    invisible(x)
}
