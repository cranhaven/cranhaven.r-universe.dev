#' Defines the initialization function for a new splitting method of \code{rpart}.
#'
#' Defines the initialization function for a new splitting method of \code{rpart}.
#'  Not to be called directly by the user. 
#'
#' @param y the response value as found in the formula that is passed in by \code{rpart}.
#'    Note that \code{rpart} will normally
#'    have removed any observations with a missing response.
#' @param offset the offset term, if any, found on the right hand side of the 
#'      formula that is passed in by \code{rpart}.
#' @param parms the vector or list (if any) supplied by the user as a
#'          \code{parms} argument to the call.
#' @param wt the weight vector from the call, if any.
#'
#' @return See reference.
#'
#' @seealso \code{\link{survs},\link{surve}}
#' @references \url{https://cran.r-project.org/package=rpart/vignettes/usercode.pdf}
#' @export


survi <- function
(y, offset, parms, wt) {
    if (!missing(offset) && length(offset) > 0)
        warning("offset argument ignored")

    sfun <- function(yval, dev, wt, ylevel, digits ) {
        paste("  mean=", format(signif(yval, digits)),
              ", MSE=" , format(signif(dev/wt, digits)),
              sep = '') }
    environment(sfun) <- .GlobalEnv
    list(y = y, parms = parms, numresp = 1, numy = ncol(y), summary = sfun)
}



