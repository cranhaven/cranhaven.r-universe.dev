#' Calculate the start date of a given ISO week
#'
#' This function takes a vector of ISO week numbers (of the form YYYYWW) and returns a Date
#' vector with the first Monday of each week. It is essentially the inverse function of
#' \code{\link{isoweek}}.
#'
#' @param x A numeric vector of ISO week numbers (of format YYYYWW)
#'
#' @return A vector of class \code{\link{Date}} and length equal to \code{x}, containing the
#'   start date (first Monday) of each ISO week.
#'
#' @examples
#' isoweekStart(201740) # Start of 2017-18 influenza surveillance
#' isoweekStart(isoweek(Sys.Date()))
#'
#' @export
isoweekStart <- function(x) {
  year <- x %/% 100
  week <- x %% 100
  x.date <- as.Date(paste(year,"-6-1", sep=""))
  x.weekday <- as.integer(format(x.date,"%w"))
  x.weekday[x.weekday==0]=7
  x.nearest.thu <- x.date-x.weekday+4
  x.isoweek <- isoweek(x.nearest.thu, type="week")
  res <- x.nearest.thu + 7*(week-x.isoweek) - 3
  if (sum(isoweek(res, type="both_num") != x)>0) stop("Error specifying ISO week number")
  return(res)
}
