#' Function to find points along a horseshoe shape
#' @param z - Distance in kilometers from the center of the horseshoe to its lower boundary
#' @param spacing - Degrees between successive point outlining the horseshoe shape
#' @param endAngles - Array of values denoting the degree of rotation for the circular part of the horseshoe distribution
#' @return Returns a 2-dimensional array of decimal degree coordinates outlining a horseshoe shape
#' @details Currently forces use 0,0 as the center, a height 5/4 width, and r2=2*r1 of origin (center)
#' @note Currently only works when endAngles are multiples of -90,90; otherwise rectangle point positions are off
#' @examples
#' PtsAlgHorseShoe(z=2000,spacing=1,endAngles=c(-90,90))
#' @references
#' [1] From http://mathforum.org/library/drmath/view/51816.html
#' @export
PtsAlgHorseShoe<-function(z,spacing=1,endAngles=c(-90,90)){
  arcD<-z/6371
  circAng<-deg2rad(endAngles[1])
  i=endAngles[1]
  latOut<-c(NULL)
  longOut<-c(NULL)
  while(i<endAngles[2]){
    circAng<-c(circAng,deg2rad(i+spacing))
    i<-i+spacing
  }
  nAng<-length(circAng)

  for(j in 1:nAng){
    latOut<-c(latOut,asin(sin(0)*cos(arcD)+cos(0)*sin(arcD)*cos(circAng[j])))
    dlon<-atan2(sin(circAng[j])*sin(arcD)*cos(0),cos(arcD)-sin(0)*sin(latOut[j]))
    longOut<-c(longOut,((0+dlon+pi)%%(2*pi))-pi)
  }

  arcDInner<-0.5*arcD
  for(j in nAng:1){
    latOut<-c(latOut,asin(sin(0)*cos(arcDInner)+cos(0)*sin(arcDInner)*cos(circAng[j])))
    dlon<-atan2(sin(circAng[j])*sin(arcDInner)*cos(0),cos(arcDInner)-sin(0)*sin(latOut[j]))
    longOut<-c(longOut,((0+dlon+pi)%%(2*pi))-pi)
  }

  arcDRec<-((2.5*z)-z)/6371
  latOut<-c(latOut,asin(sin(latOut[1])*cos(arcDRec)+cos(latOut[1])*sin(arcDRec)*cos(circAng[1]-(pi/2))))
  dlon<-atan2(sin(circAng[1]+(pi/2))*sin(arcDRec)*cos(latOut[1]),cos(arcDRec)-sin(latOut[1])*sin(latOut[nAng*2+1]))
  if(endAngles[1]%%180!=0){
    longOut<-c(longOut,((longOut[1]+dlon+pi)%%(2*pi))-pi)
  }
  if(endAngles[1]%%180==0){
    longOut<-c(longOut,((longOut[1]-dlon+pi)%%(2*pi))-pi)
  }

  latOut<-c(latOut,asin(sin(latOut[nAng])*cos(arcDRec)+cos(latOut[nAng])*sin(arcDRec)*cos(circAng[2]-(pi/2))))
  dlon<-atan2(sin(circAng[2]-(pi/2))*sin(arcDRec)*cos(latOut[nAng]),cos(arcDRec)-sin(latOut[nAng])*sin(latOut[nAng*2+2]))
  if(endAngles[1]%%180==0){
    longOut<-c(longOut,((longOut[nAng]+dlon+pi)%%(2*pi))-pi)
  }
  if(endAngles[1]%%180!=0){
    longOut<-c(longOut,((longOut[nAng]-dlon+pi)%%(2*pi))-pi)
  }

  latOut<-c(latOut,asin(sin(latOut[nAng+1])*cos(arcDRec)+cos(latOut[nAng+1])*sin(arcDRec)*cos(circAng[1]-(pi/2))))
  dlon<-atan2(sin(circAng[1]+(pi/2))*sin(arcDRec)*cos(latOut[1]),cos(arcDRec)-sin(latOut[1])*sin(latOut[nAng*2+3]))
  if(endAngles[1]%%180!=0){
    longOut<-c(longOut,((longOut[nAng+1]+dlon+pi)%%(2*pi))-pi)
  }
  if(endAngles[1]%%180==0){
    longOut<-c(longOut,((longOut[nAng+1]-dlon+pi)%%(2*pi))-pi)
  }
  latOut<-c(latOut,asin(sin(latOut[nAng*2])*cos(arcD)+cos(latOut[nAng*2])*sin(arcDRec)*cos(circAng[2]-(pi/2))))
  dlon<-atan2(sin(circAng[2]-(pi/2))*sin(arcDRec)*cos(latOut[nAng*2]),cos(arcDRec)-sin(latOut[nAng*2])*sin(latOut[nAng*2+4]))
  if(endAngles[1]%%180==0){
    longOut<-c(longOut,((longOut[nAng*2]+dlon+pi)%%(2*pi))-pi)
  }
  if(endAngles[1]%%180!=0){
    longOut<-c(longOut,((longOut[nAng*2]-dlon+pi)%%(2*pi))-pi)
  }
  edge1<-nAng+1
  edge2<-2*nAng
  latOut<-c(latOut[2*nAng+1],latOut[1:nAng],latOut[2*nAng+2],latOut[2*nAng+3],rev(latOut[edge1:edge2]),latOut[2*nAng+4])
  longOut<-c(longOut[2*nAng+1],longOut[1:nAng],longOut[2*nAng+2],longOut[2*nAng+3],rev(longOut[edge1:edge2]),longOut[2*nAng+4])

  latOut<-(180*latOut)/pi
  longOut<-(180*longOut)/pi
  res<-cbind(longOut,latOut)
  return(res)
}
