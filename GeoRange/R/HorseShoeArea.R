#' Function to calcuate the area of a given horseshoe shape
#' @param HorseShoeShape - Object containing the outline of a horseshoe shape, output from PtsAlgHorseShoe function
#' @return Returns the area of the horseshoe shape with the circular and rectangular portions separated
#' @note Currently forces use of a height 5/4 width and r2=2*r1 of origin (center)
#' @examples
#' HorseShoeTest<-PtsAlgHorseShoe(z=2000,spacing=1,endAngles=c(-90,90))
#' HorseShoeArea(HorseShoeTest)
#' @references
#' [1] From http://mathforum.org/library/drmath/view/51816.html
#' @export
HorseShoeArea<-function(HorseShoeShape){
  npts<-length(HorseShoeShape[,1])
  OutEnd<-((npts/2)-2)+1
  InnStart<-((npts/2)-2)+4
  InnEnd<-npts-1
  WestRec<-c(1,2,InnStart,npts)
  EastRec<-c(OutEnd,OutEnd+1,OutEnd+2,InnEnd)
  AreaOutCirc<-CHullAreaEarth(HorseShoeShape[2:OutEnd,1],HorseShoeShape[2:OutEnd,2])
  AreaInnCirc<-CHullAreaEarth(HorseShoeShape[InnStart:InnEnd,1],HorseShoeShape[InnStart:InnEnd,2])
  RingArea<-AreaOutCirc-AreaInnCirc
  AreaWestRec<-CHullAreaEarth(HorseShoeShape[WestRec,1],HorseShoeShape[WestRec,2])
  AreaEastRec<-CHullAreaEarth(HorseShoeShape[EastRec,1],HorseShoeShape[EastRec,2])
  RecArea<-AreaWestRec+AreaEastRec
  res<-list(Ring_Area=RingArea,Rec_Area=RecArea)
  return(res)
}
