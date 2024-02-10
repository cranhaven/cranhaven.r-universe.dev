## circle_center
## Jamie Spaulding

#' Center of the Circle Calculation for Geographic Profiling
#' @description A calculation for the center of the circle which bounds the
#'     incident coordinates for serial crime analysis. This function is among
#'     the centrographic methods which have been used for geographic profiling.
#'     The model assumes that the serial perpetrator's home base is relatively
#'     central among the crime incidents.
#' @param lat a vector of latitudes for the crime incident series
#' @param lon a vector of latitudes for the crime incident series
#' @return A latitude and longitude coordinate for the center of the circle
#'     which encompasses the incidents. This point can be used to prioritize
#'     the area which contains the offender's anchor point.
#' @author Jamie Spaulding, Keith Morris
#' @keywords spatial methods
#' @examples
#' #Using provided dataset for the Boston Strangler Incidents:
#' data(desalvo)
#' circle_center(desalvo$lat, desalvo$lon)
#' @importFrom spatstat.geom nndist.ppp
#' @importFrom spatstat.geom owin
#' @importFrom spatstat.geom ppp
#' @export
circle_center <- function(lat, lon){
  n <- length(lat)
  points <- data.frame(lat, lon)
  dat <- spatstat.geom::ppp(lon, lat, window = spatstat.geom::owin(xrange = c(min(lon), max(lon)),
                                     yrange = c(min(lat), max(lat))))
  nndists <- spatstat.geom::nndist.ppp(dat, k = n-1)
  fncases <- which(grepl(max(nndists), nndists))
  xi <- points[fncases[1],]
  xj <- points[fncases[2],]
  cc_lat <- mean(xi$lat, xj$lat)
  cc_lon <- mean(xi$lon, xj$lon)
  return(data.frame(lat = cc_lat, lon = cc_lon))
}
