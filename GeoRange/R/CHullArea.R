#' Performs Convex Hull area calculation
#' @param longs - Array of longitudinal occurrence values in decimal degrees
#' @param lats - Array of latitudinal occurrence values in decimal degrees
#' @return Returns area of a set of coordinates
#' @examples
#' longs<-c(-12,23,55)
#' lats<-c(34,22,30)
#' CHullArea(longs,lats)
#' @details Uses the cylindrical equal area projection in order to check if the minimum convex hull wraps around the prime meridian
#' @note Relies on the 'sp' package for the Polygon and chull function
#' @importFrom sp Polygon
#' @importFrom grDevices chull
#' @export
CHullArea<-function(longs,lats){
  Coords<-CoordCollapse(longs,lats)
  if(length(Coords$Longitude)>2){
    hpts <- chull(x = Coords$Longitude, y = Coords$Latitude)
    hpts <- c(hpts, hpts[1])
    xy.coords <- cbind(Coords$Longitude, Coords$Latitude)
    chull.coords <- xy.coords[hpts,]
    chull.poly <- Polygon(chull.coords, hole=F)
    chull.area <- chull.poly@area
  }
  if(length(Coords$Longitude)<3){
    chull.area<-0
  }
  return(chull.area)
}
