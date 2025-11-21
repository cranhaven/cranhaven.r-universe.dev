#' Reliability.
#'
#' @param outageTime Forced Outage Time. A numeric value representing the forced outage time or a
#' numeric vector of outage times.
#' @param totalTime Total Time. A numeric value representing the total time for a given period
#' or a numeric vector of time periods
#' @return The function returns a numeric value representing the reliability for a given period.
#' @examples
#' outage <- 100
#' total <- 1000
#' rel(outage, total)
#' @export
rel <- function(outageTime, totalTime)

  {

  # Check for a numeric vector of forced outage times
  if(all(!is.null(outageTime), !is.numeric(outageTime))) {
    stop("Argument \"outageTime\" must be a numeric vector")
  }

  # Check for a numeric vector of time periods
  if(all(!is.null(totalTime), !is.numeric(totalTime))) {
    stop("Argument \"totalTime\" must be a numeric vector")
  }

  # Calculate reliability
  rel <-  1 - (sum(outageTime)/sum(totalTime))
  print(rel)

}
