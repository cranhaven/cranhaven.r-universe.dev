#' Calculates the geodesic distance between two points specified by latitude/longitude using Vincenty inverse formula for ellipsoids
#' @param long1 - Longitudinal value of first point in decimal degrees
#' @param lat1 - Latitudinal value of first point in decimal degrees
#' @param long2 - Longitudinal value of second point in decimal degrees
#' @param lat2 - Latitudinal value of second point in decimal degrees
#' @return Returns the distance between two points on the Earth in kilometers
#' @examples
#' long1<-22
#' lat1<-44
#' long2<-52
#' lat2<-51
#' gcd_vif(long1,lat1,long2,lat2)
#' @references
#' [1] Adapted from http://www.r-bloggers.com/great-circle-distance-calculations-in-r/
#' @export

gcd_vif <- function(long1, lat1, long2, lat2) {

  long1<-deg2rad(long1)
  lat1<-deg2rad(lat1)
  long2<-deg2rad(long2)
  lat2<-deg2rad(lat2)

  a <- 6378137
  b <- 6356752.314245
  f <- 1/298.257223563

  L <- long2-long1
  U1 <- atan((1-f) * tan(lat1))
  U2 <- atan((1-f) * tan(lat2))
  sinU1 <- sin(U1)
  cosU1 <- cos(U1)
  sinU2 <- sin(U2)
  cosU2 <- cos(U2)

  cosSqAlpha <- NULL
  sinSigma <- NULL
  cosSigma <- NULL
  cos2SigmaM <- NULL
  sigma <- NULL

  lambda <- L
  lambdaP <- 0
  iterLimit <- 100
  while (abs(lambda-lambdaP) > 1e-12 & iterLimit>0) {
    sinLambda <- sin(lambda)
    cosLambda <- cos(lambda)
    sinSigma <- sqrt( (cosU2*sinLambda) * (cosU2*sinLambda) +
                        (cosU1*sinU2-sinU1*cosU2*cosLambda) * (cosU1*sinU2-sinU1*cosU2*cosLambda) )
    if (sinSigma==0) return(NA)
    cosSigma <- sinU1*sinU2 + cosU1*cosU2*cosLambda
    sigma <- atan2(sinSigma, cosSigma)
    sinAlpha <- cosU1 * cosU2 * sinLambda / sinSigma
    cosSqAlpha <- 1 - sinAlpha*sinAlpha
    cos2SigmaM <- cosSigma - 2*sinU1*sinU2/cosSqAlpha
    if (is.na(cos2SigmaM)) cos2SigmaM <- 0
    C <- f/16*cosSqAlpha*(4+f*(4-3*cosSqAlpha))
    lambdaP <- lambda
    lambda <- L + (1-C) * f * sinAlpha *
      (sigma + C*sinSigma*(cos2SigmaM+C*cosSigma*(-1+2*cos2SigmaM*cos2SigmaM)))
    iterLimit <- iterLimit - 1
  }
  if (iterLimit==0) return(NA)
  uSq <- cosSqAlpha * (a*a - b*b) / (b*b)
  A <- 1 + uSq/16384*(4096+uSq*(-768+uSq*(320-175*uSq)))
  B <- uSq/1024 * (256+uSq*(-128+uSq*(74-47*uSq)))
  deltaSigma = B*sinSigma*(cos2SigmaM+B/4*(cosSigma*(-1+2*cos2SigmaM^2) -
                                             B/6*cos2SigmaM*(-3+4*sinSigma^2)*(-3+4*cos2SigmaM^2)))
  s <- b*A*(sigma-deltaSigma) / 1000
  if(length(s)==0){
    s<-NA
  }

  return(s)
}

