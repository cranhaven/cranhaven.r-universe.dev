#' Calculates the latitudinal range in degrees and kilometers
#' @param lats - Latitudinal occurrences in decimal degrees
#' @return Returns the outermost coordinates of the set, the latitudinal span in degrees and in kilometers
#' @examples
#' lats<-c(-23,56,-2,45,66)
#' LatRg(lats)
#' @export
LatRg<-function(lats){
  nLats<-length(lats)
  if(nLats>0){
    LatSpan<-range(lats)
    LatSpan_deg<-LatSpan[2]-LatSpan[1]
    if(LatSpan_deg!=0){
      LatSpan_km<-gcd_vif(0,LatSpan[1],0.0001,LatSpan[2])
    }
    else{
      LatSpan_km<-0
    }
    res<-list(CoordSpan=LatSpan,DegSpan=LatSpan_deg,KmSpan=LatSpan_km)
  }
  else{
    res<-list(CoordSpan=0,DegSpan=0,KmSpan=0)
  }
  return(res)
}

