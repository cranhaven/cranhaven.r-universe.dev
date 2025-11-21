#' Serviceability.
#'
#' @param serviceTime Service Time. A numeric value representing the service time or a
#' numeric vector of service times.
#' @param totalTime Total Time. A numeric value representing the total time for a given period
#' or a numeric vector of time periods
#' @return The function returns a numeric value representing the serviceability factor for a given period.
#' @examples
#' service <- 900
#' total <- 1000
#' serv(service, total)
#' @export
serv <- function(serviceTime, totalTime)

{

  # Check for a numeric vector of forced outage times
  if(all(!is.null(serviceTime), !is.numeric(serviceTime))) {
    stop("Argument \"serviceTime\" must be a numeric vector")
  }

  # Check for a numeric vector of time periods
  if(all(!is.null(totalTime), !is.numeric(totalTime))) {
    stop("Argument \"totalTime\" must be a numeric vector")
  }

  # Calculate serviceability factor
  serv <-  serviceTime/totalTime
  print(serv)

}
