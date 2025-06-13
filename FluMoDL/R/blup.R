#' Get or set BLUP coefficients for a FluMoDL object
#'
#' This retrieves or sets the BLUP coefficients for a particular
#' \link[=fitFluMoDL]{FluMoDL} object.
#'
#' @param object An object of class \code{\link[=fitFluMoDL]{FluMoDL}}
#' @param value An object of class \code{\link{summary.FluMoDL}}, holding BLUP estimates
#'   to be assigned to \code{x}
#' @param ... Further arguments passed to or from other methods.
#'
#' @return For \code{blup.FluMoDL}, the returned object of class
#'   \code{\link{summary.FluMoDL}} holding the BLUP coefficients associated
#'   with the FluMoDL object.
#'
#' @importFrom mvmeta blup
#'
#' @export
blup.FluMoDL <- function(object, ...) {
  return(object$blup)
}


#' @rdname blup.FluMoDL
#' @export
`blup<-` <- function(object, value) {
  UseMethod("blup<-")
}


#' @rdname blup.FluMoDL
#' @export
`blup<-.FluMoDL` <- function(object, value) {
  if (!inherits(value, "summary.FluMoDL")) stop("'value' should be of class 'summary.FluMoDL'")
  if (!hasRSV(object) && hasRSV(value)) {
    value$coef$proxyRSV <- NULL
    value$vcov$proxyRSV <- NULL
  }
  object$blup <- addPredictions(value, object)
  return(object)
}

