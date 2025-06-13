#' Does object include a periodic B-spline term?
#'
#' This method checks whether a 'FluMoDL' object includes a
#' periodic B-spline term in its parametrization or not. 
#' By default FluMoDL objects are created with a periodic term, 
#' unless argument \code{periodic} in \code{\link{fitFluMoDL}} is set to \code{FALSE}
#'
#' @param x An object of class \code{\link[=fitFluMoDL]{FluMoDL}}
#'
#' @return \code{TRUE} if the model includes a periodic term, \code{FALSE} if it does not.
#'
#' @examples
#' data(greece) # Use example surveillance data from Greece
#' m <- with(greece, fitFluMoDL(deaths = daily$deaths,
#'     temp = daily$temp, dates = daily$date,
#'     proxyH1 = weekly$ILI * weekly$ppH1,
#'     proxyH3 = weekly$ILI * weekly$ppH3,
#'     proxyB = weekly$ILI * weekly$ppB,
#'     yearweek = weekly$yearweek))
#' hasPeriodic(m)   # Returns TRUE
#'
#' @export
hasPeriodic <- function(x) {
  UseMethod("hasPeriodic")
}


#' @export
hasPeriodic.FluMoDL <- function(x) {
  return(sum(grepl("pbs(doy,", names(x$model$model), fixed=TRUE))>0)
}

