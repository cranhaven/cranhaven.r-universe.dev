#' Print method for adeahierarchical class
#'
#' For the final model of adea_hierarchical function prints a summary, the input variables, and outputs variables.
#' 
#' @name print.adeahierarchical
#' @param x Object of class adeahierarchical to print.
#' @param ... Optional arguments to "print".
#' @method print adeahierarchical
#' @export
print.adeahierarchical <- function(x, ...) {
    models <- summary(x)
    models <- models$models
    print(setNames(models, gettext(names(models))), ...)
    invisible(x)
}
