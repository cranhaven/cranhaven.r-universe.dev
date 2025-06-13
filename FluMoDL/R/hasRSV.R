#' Does object have a term for RSV?
#'
#' This method checks whether a 'FluMoDL' or 'summary.FluMoDL' object contains a
#' \code{\link[dlnm:crossbasis]{cross-basis term}} for RSV (Respiratory Syncytial Virus)
#' incidence proxy, or contains only terms for influenza incidence proxies.
#'
#' @param x An object of class \code{\link[=fitFluMoDL]{FluMoDL}} or
#' \code{\link{summary.FluMoDL}}
#'
#' @return \code{TRUE} if the model contains a term for RSV, \code{FALSE} if it does not.
#'
#' @examples
#' data(greece) # Use example surveillance data from Greece
#' m <- with(greece, fitFluMoDL(deaths = daily$deaths,
#'     temp = daily$temp, dates = daily$date,
#'     proxyH1 = weekly$ILI * weekly$ppH1,
#'     proxyH3 = weekly$ILI * weekly$ppH3,
#'     proxyB = weekly$ILI * weekly$ppB,
#'     yearweek = weekly$yearweek))
#' hasRSV(m)   # Returns FALSE
#' hasRSV(summary(m))   # Also returns FALSE
#'
#' @export
hasRSV <- function(x) {
  UseMethod("hasRSV")
}


#' @export
hasRSV.FluMoDL <- function(x) {
  return(!is.null(x$pred$proxyRSV))
}


#' @export
hasRSV.summary.FluMoDL <- function(x) {
  return(!is.null(x$coef$proxyRSV))
}


#' @export
hasRSV.metaFluMoDL <- function(x) {
  return(!is.null(x$proxyRSV))
}

