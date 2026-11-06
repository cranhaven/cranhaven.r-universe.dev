#' summary method for fsdea class
#'
#' Extract information from a fsdea class object and compute some more.
#' The default tolerance to consider a DMU as efficient one is .001 in reports.
#' Use `eff.tolerance` parameter to consider another tolerance between 0 and 1.
#' 
#' @aliases summary.fsdea
#' @importFrom stats sd
#' @param object is the object of class dea to summarise
#' @param ... For compatibility reason, see note about `eff.tolerance` parameter.
#' @method summary fsdea
#' @return This function returns the following information, as a named vector:
#' \itemize{
#'   \item Model name: if it is previously set
#'   \item Orientation: Model orientation
#'   \item Inputs: Names of the input variables in the model
#'   \item Outputs: Names of the output variables in the model
#'   \item n: Number of DMUs
#'   \item nEfficients: Number of efficient DMUs taking into account whithin eff.tolerance tolerance.
#'   \item Mean: This and following statistical measure of the efficiency scores. Its mean.
#'   \item sd: Its standard deviation.
#'   \item Min.: Its minimun value.
#'   \item 1st Qu.: Its first quater.
#'   \item Median: Its median.
#'   \item 3rd Qu.: Its third quater.
#'   \item Max.: Its maximum value.
#'   \item nInputs: Number of input variables to select
#'   \item nOutputs: Number of output variables to select
#'   \item nTotal: Total number of variables to select
#'   \item iSelected: Names of the selected input variables
#'   \item oSelected: Names of the selected output variables
#' }

#' @export
summary.fsdea <- function(object, ...) {
    ## Check input parameters
    args <- list(...)
    eff.tolerance <- args$eff.tolerance
    ## Check if eff.tolerance is missing
    if (is.null(eff.tolerance)) {
        eff.tolerance <- .001
    } else {
        if (!is.numeric(eff.tolerance) || eff.tolerance < 0 || eff.tolerance > 1) stop(paste('summary.fsdea:summary.fsdea.R:35', gettext('eff.tolerance is not a number between 0 and 1')))
    }
    ## Get information already in dea
    s <- summary.dea(object, eff.tolerance = eff.tolerance)
    ## Add new information
    s['Inputs'] <- object$Inputs
    s['Outputs'] <- object$Outputs
    s['nTotal'] <- object$nTotal
    s['iSelected'] <- paste(object$inputnames[object$iselected == 1], collapse = ', ')
    s['oSelected'] <- paste(object$outputnames[object$oselected == 1], collapse = ', ')
    ## Setup class
    class(s) <- 'summary.fsdea'
    ## return
    s
}

#' @export
#' @aliases print.summay.fsdea
print.summary.fsdea <- function(x, ...) {
    lx <- data.frame(unlist(x))
    names(lx) <- ''
    rownames(lx) <- gettext(rownames(lx))
    print(lx, ...)
    invisible(x)
}
