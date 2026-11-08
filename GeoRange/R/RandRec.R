#' Function to randomly generate points within a given rectangular shaped distribution
#' @param RecShape - The outline of a rectangular distribution output from the EqualAreaRectangle() function
#' @param npts - The number of randomly generated points within the rectangular shape
#' @return Returns a 2-dimensional array of decimal degree coordinates within a rectangular shape
#' @note Function currently does not take acount of the decreasing surface area moving toward the poles so points closer to the poles will be overrepresented relative to the actual surface area they represent
#' @examples
#' HorseShoeTest<-PtsAlgHorseShoe(z=2000,spacing=1,endAngles=c(-90,90))
#' HorseShoePts<-RandHorseShoe(center=c(0,0),npts=100,HorseShoeShape=HorseShoeTest)
#' RecOutline<-EqualAreaRectangle(TargetArea=as.numeric(HorseShoePts$TotalArea_km2),error=0.001)
#' RandRec(RecShape=RecOutline,npts=100)
#' @importFrom stats runif
#' @export
RandRec<-function(RecShape,npts=100){
  RandLong<-runif(npts,min=min(RecShape[,1]),max=max(RecShape[,1]))
  RandLat<-runif(npts,min=min(RecShape[,2]),max=max(RecShape[,2]))
  CordsRec<-list(Coords=cbind(RandLong,RandLat))
  return(CordsRec)
}
