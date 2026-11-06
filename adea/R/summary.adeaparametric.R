#' Summary method for adeaparametric class
#'
#' For the final model of adea_parametric function print the model name, orientation, load orientation, a summary, the input variables, and outputs variables.
#'
#' The default tolerance to consider a DMU as efficient one is .001 in reports.
#' Use `eff.tolerance` parameter to consider another tolerance between 0 and 1.
#' 
#' @param object is the object of class adeaparametric to summarise
#' @param ... For compatibility reason, see note about `eff.tolerance` parameter.
#' @method summary adeaparametric
#' @export
summary.adeaparametric <- function(object, ...) {
    ## Check input parameters
    args <- list(...)
    eff.tolerance <- args$eff.tolerance
    ## Check if eff.tolerance is missing
    if (is.null(eff.tolerance)) {
        eff.tolerance <- .001
    } else {
        if (!is.numeric(eff.tolerance) || eff.tolerance < 0 || eff.tolerance > 1) stop(paste('summary.adeaparametric:summary.adeaparametric.R:18', gettext('eff.tolerance is not a number between 0 and 1')))
    }
    ## Do the job
    l <- summary.adeahierarchical(object, eff.tolerance = eff.tolerance)
    class(l) <- 'summary.adeaparametric'
    l
}

#' @export
print.summary.adeaparametric <- function(x, ...) {
    print.summary.adeastepwise(x, ...)
    invisible(x)
}
