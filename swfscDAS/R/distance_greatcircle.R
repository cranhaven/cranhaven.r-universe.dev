#' Calculate great-circle distance
#'
#' Calculate the great-circle distance between two lat/lon points
#'
#' @param lat1 numeric; starting latitude coordinate(s)
#' @param lon1 numeric; starting longitude coordinate(s)
#' @param lat2 numeric; ending latitude coordinate(s)
#' @param lon2 numeric; ending longitude coordinate(s)
#'
#' @return Distance in kilometers between lat1/lon1 and lat2/lon2
#'
#' @seealso \url{https://en.wikipedia.org/wiki/Great-circle_distance}
#'
#' @export
distance_greatcircle <- function(lat1, lon1, lat2, lon2) {
  # From EAB and KAF
  stopifnot(
    inherits(lat1, c("numeric", "integer")),
    inherits(lon1, c("numeric", "integer")),
    inherits(lat2, c("numeric", "integer")),
    inherits(lon2, c("numeric", "integer"))
  )

  R <- pi/180 #angle in radians = angle in degrees * R
  D <- 180/pi #angle in degrees = angle in radains * D
  dist <- 0

  NAcheck <- sum(is.na(c(lat1, lon1, lat2, lon2)))
  if (NAcheck == 0) { #only continue if no NA positions
    if ((lat1 != lat2) | (lon1 != lon2))  {
      dlat1 <- lat1 * R # convert to radian values:
      dlng1 <- lon1 * R
      dlat2 <- lat2 * R
      dlng2 <- lon2 * R
      las <- sin(dlat1) * sin(dlat2) # compute distance
      lac <- cos(dlat1) * cos(dlat2) * cos(dlng1 - dlng2)
      laf <- las + lac
      if (laf < -1) {
        laf <- -1
        dacos <- (pi/2) - atan(laf/sqrt(1-(laf*laf)))
      } else if (laf < 1) {
        dacos <- (pi/2) - atan(laf/sqrt(1-(laf*laf)));
      } else {
        stop('laf value out of bounds')
      }
      dist <- (dacos * D * 60) * 1.852 #calculate distance in km
    }
  }

  dist
}
