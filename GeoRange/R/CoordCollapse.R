#' Removes duplicate geographic locations and binds coordinates into a single element
#' @param longs - Longitudinal coordinates of occurrences in decimal degrees
#' @param lats - Latitudinal coordinates of occurrences in decimal degrees
#' @return Returns a 2-column array of coordinates without any duplicate locations
#' @examples
#' longs<-c(34,133,-45)
#' lats<-c(-12,44,76)
#' CoordCollapse(longs,lats)
#' @note Points are truncated to the hundredths place before checking for duplicates
#' @export
CoordCollapse<-function(longs,lats){
  NSites<-length(lats)
  lats<-lats[order(longs)]
  longs<-sort(longs)
  dups<-c()
  if(NSites>1){
    for(i in 2:NSites){
      if(longs[i]==longs[i-1]){
        i_2<-i-1
        LongDupPos<-which(longs[1:i_2]==longs[i])
        if(any(lats[i]==lats[LongDupPos])){
          dups<-c(dups,i)
        }
      }
    }
  }
  if(length(dups)!=0){
    longs_collapsed<-longs[-dups]
    lats_collapsed<-lats[-dups]
  }
  else{
    longs_collapsed<-longs
    lats_collapsed<-lats
  }
  res<-data.frame(Longitude=longs_collapsed,Latitude=lats_collapsed)
  return(res)
}
