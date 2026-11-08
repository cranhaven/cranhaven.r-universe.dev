#' Create a rectangular shaped distribution with equal area to a given area
#' @param center - Arrary containing the coordinates of the center of circular portion of the rectangle in decimal degree
#' @param TargetArea - Area in square kilometers desired for the rectangle
#' @param error - The tolerable proportion of error between the rectangular shape and the TargetArea
#' @return Returns a 2-dimensional array of decimal degree coordinates outlining a rectangular shaped distribution
#' @note This returns 100 evenly spaced points along each corner of the rectangle, in addition to the corners themselves
#' @examples
#' HorseShoeTest<-PtsAlgHorseShoe(z=2000,spacing=1,endAngles=c(-90,90))
#' HorseShoePts<-RandHorseShoe(center=c(0,0),npts=100,HorseShoeShape=HorseShoeTest)
#' EqualAreaRectangle(TargetArea=as.numeric(HorseShoePts$TotalArea_km2),error=0.001)
#' @export
EqualAreaRectangle<-function(center=c(0,0),TargetArea,error=0.001){
  Ratio<-100
  temp<-sample.int(10,size=1)
  LongRec<-c(NULL)
  LatRec<-c(NULL)
  testArea<-c(NULL)
  TopErr<-1+error/2
  BotErr<-1-error/2
  while(Ratio<BotErr | Ratio>TopErr){
    LongRec<-c(center[1]-temp,center[1]+temp,center[1]-temp,center[1]+temp)
    LatRec<-c(center[2]+temp,center[2]+temp,center[2]-temp,center[2]-temp)
    testArea<-CHullAreaEarth(LongRec,LatRec)
    Ratio<-testArea/TargetArea
    if(Ratio>TopErr){
      temp<-0.5*temp
    }
    if(Ratio<BotErr){
      temp<-1.25*temp
    }
  }
  topLong<-seq(from=min(LongRec),to=max(LongRec),by=(max(LongRec)-min(LongRec))/100)
  minEdgeLat<-rep(min(LatRec),length(topLong))
  maxEdgeLat<-rep(max(LatRec),length(topLong))
  LongRec<-c(LongRec,topLong,topLong)
  LatRec<-c(LatRec,minEdgeLat,maxEdgeLat)
  topLat<-seq(from=min(LatRec),to=max(LatRec),by=(max(LatRec)-min(LatRec))/100)
  minEdgeLong<-rep(min(LongRec),length(topLat))
  maxEdgeLong<-rep(max(LongRec),length(topLat))
  LongRec<-c(LongRec,minEdgeLong,maxEdgeLong)
  LatRec<-c(LatRec,topLat,topLat)
  print(TargetArea)
  print(testArea)
  res<-cbind(LongRec,LatRec)
  return(res)
}
