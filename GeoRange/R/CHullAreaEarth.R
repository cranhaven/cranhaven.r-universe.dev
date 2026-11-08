#' Performs convex hull area calculation from coordinate sets on the Earth's surface
#' @param longs - Longitudinal coordinates of occurrences in decimal degrees
#' @param lats - Latitudinal coordinates of occurrences in decimal degrees
#' @return Returns the convex hull area is square kilometers
#' @examples
#' longs<-c(-133,-101,56)
#' lats<-c(33,12,-2)
#' CHullAreaEarth(longs,lats)
#' @details Uses the cylindrical equal area projection in order to check if the minimum convex hull wraps around the prime meridian
#' @note Relies on the 'sp' package for the Polygon and chull function. Assumes latitude and longitude coordinates use the WGS84 datum
#' @importFrom proj4 project
#' @importFrom stats runif
#' @export
CHullAreaEarth<-function(longs,lats){
  Coords<-CoordCollapse(longs,lats)
  NLats<-length(Coords[,2])
  TotArea<-0
  if(length(Coords$Longitude)>2){
    for(t in 1:NLats){
      Coords$Longitude[t]<-Coords$Longitude[t]+runif(1,min=-0.0001,max=0.0001)
      Coords$Latitude[t]<-Coords$Latitude[t]+runif(1,min=-0.0001,max=0.0001)
    }
    CoordsProj<-project(cbind(Coords[,1],Coords[,2]),proj = "+proj=cea +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=km +no_defs")
    maxLong<-max(CoordsProj[,1])
    minLong<-min(CoordsProj[,1])
    if(sign(minLong)!=sign(maxLong)){
      MintoMax<-abs(minLong)+abs(maxLong)
      if(MintoMax>7795.67){
        for(k in 1:NLats){
          if(CoordsProj[k,1]<0){
            CoordsProj[k,1]<-CoordsProj[k,1]+31182.68
          }
        }
      }
    }
    TotArea<-CHullArea(CoordsProj[,1],CoordsProj[,2])
  }
  if(length(Coords$Longitude)<3){
    TotArea<-0
  }
  return(TotArea)
}
