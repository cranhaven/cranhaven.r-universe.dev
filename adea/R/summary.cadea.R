#' summary method for cadea class
#' 
#' Extract information from a fsdea class object and compute some more.
#' The default tolerance to consider a DMU as efficient one is .001 in reports.
#' Use `eff.tolerance` parameter to consider another tolerance between 0 and 1.
#' 
#' @aliases summary.cadea
#' @importFrom stats sd
#' @param object is the object of class cadea to summarise
#' @param ... For compatibility reason, see note about `eff.tolerance` parameter.
#' @method summary cadea
#' @export
summary.cadea <- function(object, ...) {
    ## Check input parameters
    args <- list(...)
    eff.tolerance <- args$eff.tolerance
    ## Check if eff.tolerance is missing
    if (is.null(eff.tolerance)) {
        eff.tolerance <- .001
    } else {
        if (!is.numeric(eff.tolerance) || eff.tolerance < 0 || eff.tolerance > 1) stop(paste('summary.cadea:summary.cadea.R:17', gettext('eff.tolerance is not a number between 0 and 1')))
    }
    ## Summary dea information
    ss <- summary.adea(object, eff.tolerance = eff.tolerance)
    ss[['Minimum for loads']] <- object$loads$load.min
    ss[['Maximum for loads']] <- object$loads$load.max
    ss <- c(ss[1:6], ss[20:21], ss[7:19])
    class(ss) <- 'summary.cadea'
    ss
}

#' @export
#' @aliases print.summay.cadea
print.summary.cadea <- function(x, ...) {
    lx <- data.frame(unlist(x))
    names(lx) <- ''
    rownames(lx) <- gettext(rownames(lx))
    print(lx, ...)
    invisible(x)
}
