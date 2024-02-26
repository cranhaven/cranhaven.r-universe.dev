#' @title Summarizes (or elaborates on) S3 objects in this package.
#'
#' @description Prints the (name of the) instances performing worse than expected
#' in a "funnelplot" object at the specified confidence levels.
#'
#'
#' @param object S3 object to summarize
#' @param ... extra parameters
#'
#' @return A list with:
#' \itemize{
#' \item \code{$call} The call used to obtain the input object,
#' \item \code{$'0.xx'} The detected instances at specified confidence level.
#' }
#'
#' @seealso , \code{\link[cgrcusum:funnelplot]{funnelplot}}
#'
#' @describeIn summary Summarize instances detected by the funnelplot object
#' @export
summary.funnelplot <- function(object, ...){
  k <- object$conflev
  outp <- list("call" = object$call)
  for(i in k){
    temp <- sort(object$data[which(object$data[, as.character(i)] == "worse"), "instance"])
    outp[[as.character(i)]] = droplevels(as.factor(temp))
  }
  return(outp)
}
