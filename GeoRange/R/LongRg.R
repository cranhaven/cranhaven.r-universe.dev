#' Calculates the longitudinal range in degrees and kilometers, assuming a latitude of 45 degrees for all points by default. Accounts for the possibility of wrapping around the globe.
#' @param longs - Longitudinal occurrences in decimal degrees
#' @param lats - A single value representing the latitude to calculate longitudinal distance from or a list of latitudinal coordinates in decimal degrees
#' @return Returns the outermost coordinates of the set, the longitudinal span in degrees and in kilometers
#' @details Calculates the longitudinal range as 360-largest longitudinal gap and accounts for the possbility that a taxon's range wraps around the prime meridian
#' @examples
#' longs<-c(133,76,-77,7,-80)
#' lats<-c(45)
#' LongRg(longs)
#' @importFrom stats median
#' @export

LongRg<-function(longs,lats=45){
  if(length(unique(longs))==1 & is.na(longs[1])==FALSE){
    res<-list(CoordSpan=c(longs[1],longs[1]),DegSpan=0,KmSpan=0)
    return(res)
  }
  else if(length(unique(longs))==0 | is.na(longs[1])==TRUE){
    res<-list(CoordSpan=c(NA,NA),DegSpan=NA,KmSpan=NA)
    return(res)
  }
  else{
    LongOrd<-sort(longs)
    nLongs<-length(longs)
    longGaps<-c(NULL)
    for(i in 2:nLongs){
      longGaps<-c(longGaps,LongOrd[i]-LongOrd[i-1])
    }
    longGaps<-c(longGaps,LongOrd[1]-LongOrd[nLongs])
    longGaps[nLongs]<-longGaps[nLongs]+360
    maxGap<-max(longGaps)
    LongSpan_deg<-360-maxGap
    LongSpanMax<-which(longGaps==maxGap)
    if(LongSpanMax[1]==nLongs){
      LongSpan<-c(1,nLongs)
    }
    if(LongSpanMax[1]!=nLongs){
      LongSpan<-c(LongSpanMax[1],LongSpanMax[1]+1)
    }
    if(length(lats)>1){
      medLat<-median(lats)
      if(LongSpan_deg<=180){
        LongSpan_km<-gcd_vif(0,medLat,LongSpan_deg,medLat)
      }
      else{
        LongSpan_km<-(gcd_vif(0,medLat,180,medLat)*2)-gcd_vif(0,medLat,LongSpan_deg,medLat)
      }
    }
    else{
      if(LongSpan_deg<=180){
        LongSpan_km<-gcd_vif(0,lats,LongSpan_deg,lats)
      }
      else{
        LongSpan_km<-(gcd_vif(0,lats,180,lats)*2)-gcd_vif(0,lats,LongSpan_deg,lats)
      }
    }
    res<-list(CoordSpan=c(LongOrd[LongSpan[1]],LongOrd[LongSpan[2]]),DegSpan=LongSpan_deg,KmSpan=LongSpan_km)
    return(res)
  }
}
