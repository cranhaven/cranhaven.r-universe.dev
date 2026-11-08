#' Calculates the geodesic distance between two points specified by latitude and longitude using the Haversine formula
#' @param long1 - Longitudinal value of first point in decimal degrees
#' @param lat1 - Latitudinal value of first point in decimal degrees
#' @param long2 - Longitudinal value of second point in decimal degrees
#' @param lat2 - Latitudinal value of second point in decimal degrees
#' @return Returns the distance between two points on the Earth in kilometers
#' @details The Haversine formula can be inaccurate depending on coordinates
#' @examples
#' long1<-22
#' lat1<-44
#' long2<-52
#' lat2<-51
#' gcd_hf(long1,lat1,long2,lat2)
#' @note The haversine method is inaccuarate and should only be used when the vicenty formula fails or over very small distances
#' @references
#' [1] Adapted from http://www.r-bloggers.com/great-circle-distance-calculations-in-r/
#' @export
gcd_hf <- function(long1, lat1, long2, lat2) {
  long1<-deg2rad(long1)
  lat1<-deg2rad(lat1)
  long2<-deg2rad(long2)
  lat2<-deg2rad(lat2)
  R <- 6371 # Earth mean radius [km]
  delta.long <- (long2 - long1)
  delta.lat <- (lat2 - lat1)
  a <- sin(delta.lat/2)^2 + cos(lat1) * cos(lat2) * sin(delta.long/2)^2
  c <- 2 * asin(min(1,sqrt(a)))
  d = R * c
  return(d) # Distance in km
}

