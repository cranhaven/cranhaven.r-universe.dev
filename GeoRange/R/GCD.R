#' Calculates the maximum pairwise great circle distance from a set of decimal degree coordinates
#' @param longs - Longitudinal coordinates of occurrences in decimal degrees
#' @param lats - Latitudinal coordinates of occurrences in decimal degrees
#' @return Returns the maximum great circle distance in kilometers
#' @details Because this function does not account for the possibility that a taxa may wrap around more than half the Earth the maximum value is half the circumference of the Earth, approximately 20,038 kilometers.
#' @note The great circle distance can be extracted from the result of a minium spanning tree calcualation MSTDist() if available to avoid redundant calculations
#' @examples
#' longs<-c(34,156,-78)
#' lats<-c(45,12,9)
#' GCD(longs,lats)
#' @export
GCD<-function(longs,lats){
  if(length(longs)<2){
    return(NA)
  }
  else{
    Coords<-CoordCollapse(longs,lats)
    gcdMatrix<-PWMatrix(Coords)
    gcdRes<-max(gcdMatrix,na.rm=TRUE)
  }
  return(gcdRes)
}
