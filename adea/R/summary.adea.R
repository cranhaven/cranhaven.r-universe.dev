#' summary method for adea class
#' 
#' Extract information from a fsdea class object and compute some more.
#' The default tolerance to consider a DMU as efficient one is .001 in reports.
#' Use `eff.tolerance` parameter to consider another tolerance between 0 and 1.
#' 
#' @aliases summary.adea
#' @importFrom stats sd
#' @param object is the object of class adea to summarise
#' @param ... For compatibility reason, see note about `eff.tolerance` parameter.
#' @method summary adea
#' @export
summary.adea <- function(object, ...) {
    ## Check input parameters
    args <- list(...)
    eff.tolerance <- args$eff.tolerance
    ## Check if eff.tolerance is missing
    if (is.null(eff.tolerance)) {
        eff.tolerance <- .001
    } else {
        if (!is.numeric(eff.tolerance) || eff.tolerance < 0 || eff.tolerance > 1) stop(paste('summary.adea:summary.adea.R:18', gettext('eff.tolerance is not a number between 0 and 1')))
    }
    ## Summary dea information
    ss <- summary.dea(object, eff.tolerance = eff.tolerance)
    s <- list()
    s['Model name'] <- ss['Model name']
    ss['Model name'] <- NULL
    s['Orientation'] <- ss['Orientation']
    ss['Orientation'] <- NULL
    s['Load orientation'] <- object$load.orientation
    s['Model load'] <- object$loads$load
    s[['Input load']] <- object$loads$input
    s[['Output load']] <- object$loads$output
    s <- c(s, ss)
    class(s) <- 'summary.adea'
    s
}

#' @export
#' @aliases print.summay.adea
print.summary.adea <- function(x, ...) {
    lx <- data.frame(unlist(x))
    names(lx) <- ''
    rownames(lx) <- gettext(rownames(lx))
    print(lx, ...)
    invisible(x)
}
