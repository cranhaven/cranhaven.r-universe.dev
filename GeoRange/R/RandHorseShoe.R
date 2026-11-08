#' Function to randomly generate points within a horseshoe-shape
#' @param center - Arrary containint the coordinates of the center of circular portion of the horseshoe in decimal degrees
#' @param npts - Integer value indicating the number of points to generate within the horseshoe shape
#' @param HorseShoeShape - Object containing the outline of a horseshoe shape, output from PtsAlgHorseShoe function
#' @param HRatio - The ratio of the lower rectangle portions of the horseshoe to the outer radius of the circular portion of the horseshoe
#' @param RadRatio - The ration of the of the outer to inner radius of the circular part of the horseshoe shape
#' @return Returns a 2-dimensional array of decimal degree coordinates within the horseshoe shape and the total area of the shape
#' @note HRatio Currently defaults to 3/2 per the PtsAlgHorseShoe() function
#' RadRatio currently defaults to 1/2 per the PtsAlgHorseShoe() function
#' Center currently defaults to c(0,0) as this is always the center of the PtsAlgHorseShoe() function currently
#' Function currently does not take acount of the decreasing surface area moving toward the poles so points closer to the poles will be overrepresented relative to the actual surface area they represent
#' @examples
#' HorseShoeTest<-PtsAlgHorseShoe(z=2000,spacing=1,endAngles=c(-90,90))
#' RandHorseShoe(center=c(0,0),npts=100,HorseShoeShape=HorseShoeTest)
#' @importFrom stats runif
#' @export
RandHorseShoe<-function(center=c(0,0),npts=100,HorseShoeShape,HRatio=1.5,RadRatio=0.5){
  RandLong<-c(NULL)
  RandLat<-c(NULL)
  nLong<-1
  nCords<-length(HorseShoeShape[,1])
  nCordsCirc<-(nCords-4)/2
  CircWestBord<-as.double(HorseShoeShape[2,1])
  CircEastBord<-as.double(HorseShoeShape[((nCords/2)-2)+1,1])
  CircMaxLat<-max(HorseShoeShape[,2])
  CircMinLat<-HorseShoeShape[2,2]
  RecWestBord1<-CircWestBord
  RecEastBord1<-as.double(HorseShoeShape[nCords,1])
  RecWestBord2<-as.double(HorseShoeShape[nCords-1,1])
  RecEastBord2<-CircEastBord
  RecMaxLat<-HorseShoeShape[2,2]
  RecMinLat<-min(HorseShoeShape[1,2])
  Areas<-HorseShoeArea(HorseShoeShape)
  TotalArea<-Areas$Ring_Area+Areas$Rec_Area
  PropRing<-Areas$Ring_Area/TotalArea
  PropRec<-Areas$Rec_Area/TotalArea
  while(nLong!=npts){
    part<-runif(1,0,1)
    OutDist<-as.double(gcd_vif(center[1],center[2],CircWestBord,center[2]))
    InnDist<-0.5*OutDist
    if(part<=PropRing){
      tempLong<-runif(1,min=CircWestBord,max=CircEastBord)
      tempLat<-runif(1,min=CircMinLat,max=CircMaxLat)
      tempDist<-as.double(gcd_vif(center[1],center[2],tempLong,tempLat))
      RingCount<-0
      while(RingCount==0){
        if(tempDist>=InnDist & tempDist<=OutDist){
          RandLong<-c(RandLong,tempLong)
          RandLat<-c(RandLat,tempLat)
          nLong<-length(RandLong)
          RingCount<-1
        }
        else if(tempDist<InnDist | tempDist>OutDist){
          tempLong<-runif(1,min=CircWestBord,max=CircEastBord)
          tempLat<-runif(1,min=CircMinLat,max=CircMaxLat)
          tempDist<-as.double(gcd_vif(center[1],center[2],tempLong,tempLat))
        }
      }
    }
    if(part>PropRing){
      side<-runif(1,0,1)
      if(side<=0.5){
        tempLong<-runif(1,min=RecWestBord1,max=RecEastBord1)
        tempLat<-runif(1,min=RecMinLat,max=RecMaxLat)
        RandLong<-c(RandLong,tempLong)
        RandLat<-c(RandLat,tempLat)
        nLong<-length(RandLong)
      }
      if(side>0.5){
        tempLong<-runif(1,min=RecWestBord2,max=RecEastBord2)
        tempLat<-runif(1,min=RecMinLat,max=RecMaxLat)
        RandLong<-c(RandLong,tempLong)
        RandLat<-c(RandLat,tempLat)
        nLong<-length(RandLong)
      }
    }
  }
  Cords<-cbind(RandLong,RandLat)
  res<-list(TotalArea_km2=TotalArea,Coords=Cords)
  return(res)
}
