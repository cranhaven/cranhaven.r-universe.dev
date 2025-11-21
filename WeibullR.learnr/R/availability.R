#' Availability.
#'
#' @param unavailTime Unavailable Time. A numeric value representing the unavailable time or a
#' numeric vector of unavailable times.
#' @param totalTime Total Time. A numeric value representing the total time for a given period
#' or a numeric vector of time periods
#' @return The function returns a numeric value representing the availability for a given period.
#' @examples
#' unavail <- 100
#' total <- 1000
#' avail(unavail, total)
#' @export
avail <- function(unavailTime, totalTime)

{

  # Check for a numeric vector of unavailable times
  if(all(!is.null(unavailTime), !is.numeric(unavailTime))) {
    stop("Argument \"unavailTime\" must be a numeric vector")
  }

  # Check for a numeric vector of time periods
  if(all(!is.null(totalTime), !is.numeric(totalTime))) {
    stop("Argument \"totalTime\" must be a numeric vector")
  }

  # Calculate availability
  avail <-  1 - (sum(unavailTime)/sum(totalTime))
  print(avail)

}
