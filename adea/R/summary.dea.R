#' summary method for dea class
#'
#' Extract information from a dea class object and compute some more.
#' 
#' The default tolerance to consider a DMU as efficient one is .001 in reports.
#' Use `eff.tolerance` parameter to consider another tolerance between 0 and 1.
#' 
#' @aliases summary.dea
#' @importFrom stats sd
#' @param object is the object of class dea to summarise
#' @param ... For compatibility reason, see note about `eff.tolerance` parameter.
#' @method summary dea
#' @return This function returns the following information, as a named list:
#' \itemize{
#'   \item Model name: if it is previously set
#'   \item Orientation: Model orientation
#'   \item Inputs: Names of the input variables in the model
#'   \item Outputs: Names of the output variables in the model
#'   \item nInputs: Number of the input variables in the model
#'   \item nOutputs: Number of the output variables in the model
#'   \item nVariables: Total number of input and output variables in the model
#'   \item n: Number of DMUs
#'   \item nEfficients: Number of efficient DMUs taking into account whithin eff.tolerance tolerance.
#'   \item Mean: Mean of efficiency scores.
#'   \item sd: Standard deviation of efficiency scores.
#'   \item Min.: Minimun value of efficiency scores.
#'   \item 1st Qu.: First quater of efficiency scores.
#'   \item Median: Median of efficiency scores.
#'   \item 3rd Qu.: Third quater of efficiency scores.
#'   \item Max.: Maximum value of efficiency scores.
#' }

#' @export
summary.dea <- function(object, ...) {
    ## Check input parameters
    args <- list(...)
    eff.tolerance <- args$eff.tolerance
    ## Check if eff.tolerance is missing
    if (is.null(eff.tolerance)) {
        eff.tolerance <- .001
    } else {
        if (!is.numeric(eff.tolerance) || eff.tolerance < 0 || eff.tolerance > 1) stop(paste('summary.dea:summary.dea.R:42', gettext('eff.tolerance is not a number between 0 and 1')))
    }
    ## Show information already in dea
    o <- list()
    o['Model name'] <- object$name
    o['Orientation'] <- object$orientation
    o['Inputs'] <- paste(object$inputnames, collapse = ' ')
    o['Outputs'] <- paste(object$outputnames, collapse = ' ')
    o['nInputs'] <- length(object$inputnames)
    o['nOutputs'] <- length(object$outputnames)
    o['nVariables'] <- o[['nInputs']]  + o[['nOutputs']]
    ## Compute new information
    o['nEfficients'] <- neff.dea(object, eff.tolerance = eff.tolerance)
    s <- summary(object$eff)
    s <- c(s[4], sd(object$eff), s[1:3], s[5:6])
    names(s)[2] <- 'sd'
    names(s) <- paste('Eff.', names(s))
    ## Join both to return
    s <- c(o, s)
    class(s) <- 'summary.dea'
    s
}

#' @export
#' @aliases print.summary.dea
print.summary.dea <- function(x, ...) {
    lx <- data.frame(unlist(x))
    names(lx) <- ''
    rownames(lx) <- gettext(rownames(lx))
    print(lx, ...)
    invisible(x)
}

neff.dea <- function(object, eff.tolerance) {
    sum(abs(object$eff - 1) < eff.tolerance)
}
