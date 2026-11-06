#' Summary method for adeastepwise class
#'
#' For the final model of adea_stepwise function print the model name, orientation, load orientation, a summary, the input variables, and outputs variables.
#' 
#' The default tolerance to consider a DMU as efficient one is .001 in reports.
#' Use `eff.tolerance` parameter to consider another tolerance between 0 and 1.
#' 
#' @param object Is the object of class adeastepwise to summarise.
#' @param ... For compatibility reason, see note about `eff.tolerance` parameter.
#' @method summary adeastepwise
summary.adeastepwise <- function(object, ...) {
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
    l <- list()
    l['Model name'] <- object$name
    l['Orientation'] <- object$orientation
    l['Load orientation'] <- object$load.orientation
    neff <- sapply(object$models, neff.dea, eff.tolerance = eff.tolerance)
    s <- data.frame(
        'Loads' = object$loads,
        'nEfficients' = neff,
        'nVariables' = object$nvariables,
        'nInputs' = object$ninputs,
        'nOutputs' = object$noutputs,
        'Inputs' = object$inputnames,
        'Outputs' = object$outputnames)
    s <- s[s[,1] > 0,]
    s <- s[nrow(s):1,]
    l$models <- s
    class(l) <- 'summary.adeastepwise'
    l
}

#' @export
print.summary.adeastepwise <- function(x, ...) {
    models <- x$models
    lx <- x
    lx$models <- NULL
    lx <- data.frame(unlist(lx))
    rownames(lx) <- gettext(rownames(lx))
    names(lx) <- ''
    print(lx, ...)
    names(models) <- gettext(names(models))
    print(models, ...)
    invisible(x)
}
