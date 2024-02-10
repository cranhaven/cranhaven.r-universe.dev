## harm_mean_pred
## Jamie Spaulding

#' Calculation of Harmonic Mean for Geographic Profiling
#' @description A calculation of the harmonic mean for serial crime
#'     analysis. This function is among the centrographic methods which have
#'     been used for geographic profiling. The model assumes that the serial
#'     perpetrator's home base is relatively central among the crime incidents.
#' @param lat a vector of latitudes for the crime incident series
#' @param lon a vector of latitudes for the crime incident series
#' @return A latitude and longitude point of the harmonic mean of the incidents.
#'     This mean can be used to prioritize the area which contains the offender's
#'     anchor point.
#' @author Jamie Spaulding, Keith Morris
#' @keywords spatial methods
#' @examples
#' #Using provided dataset for the Boston Strangler Incidents:
#' data(desalvo)
#' harm_mean_pred(desalvo$lat, desalvo$lon)
#' @export
harm_mean_pred <- function(lat, lon){
  n <- length(lat)
  harm_lat <- 1 / mean(1 / lat)
  harm_lon <- 1 / mean(1 / lon)
  return(cbind(harm_lat, harm_lon))
}
