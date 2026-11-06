#' Print method for adeaparametric class
#'
#' For the final model of adea_parametric function prints a summary, the input variables, and outputs variables.
#' 
#' @name print.adeaparametric
#' @param x Object of class adeaparametric to print.
#' @param ... Optional arguments to "print".
#' @method print adeaparametric
#' @export
print.adeaparametric <- function(x, ...) {
    models <- summary(x)
    models <- models$models
    print(setNames(models, gettext(names(models))), ...)
    invisible(x)
}
