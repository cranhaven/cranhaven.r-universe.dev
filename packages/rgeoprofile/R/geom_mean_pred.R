## geom_mean_pred
## Jamie Spaulding

#' Calculation of Geometric Mean for Geographic Profiling
#' @description A calculation of the geometric mean for serial crime
#'     analysis. This function is among the centrographic methods which have
#'     been used for geographic profiling. The model assumes that the serial
#'     perpetrator's home base is relatively central among the crime incidents.
#' @param lat a vector of latitudes for the crime incident series
#' @param lon a vector of latitudes for the crime incident series
#' @return A latitude and longitude point of the geometric mean of the incidents.
#'     This mean can be used to prioritize the area which contains the offender's
#'     anchor point.
#' @author Jamie Spaulding, Keith Morris
#' @keywords spatial methods
#' @examples
#' #Using provided dataset for the Boston Strangler Incidents:
#' data(desalvo)
#' geom_mean_pred(desalvo$lat, desalvo$lon)
#' @export
geom_mean_pred <- function(lat, lon){
  n <- length(lat)
  geom_lat <- if (prod(lat) < 0) {
    (abs(prod(lat)) ^ (1 / n)) * -1} else {prod(lat) ^ (1 / n)}
  geom_lon <- if (prod(lon) < 0) {
    (abs(prod(lon)) ^ (1 / n)) * -1} else {prod(lon) ^ (1 / n)}
  return(cbind(geom_lat,geom_lon))
}
