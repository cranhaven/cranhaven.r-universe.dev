#' Plots the minimum spanning tree of a set of coordinates
#' @param MSTCalc - Output from the MSTDist function
#' @param color - Color of the lines connecting point
#' @param symbol - Symbol value for the pch graphical parameter for plotting coordinates
#' @param xlimit - Array of values denoting the x-axis limits
#' @param ylimit - Array of values denoting the y-axis limits
#' @return Plots a minimum spanning tree
#' @note If the xlimit and ylimit parameters are left to their default values the axis ranges are based on the minimum and maximum values of the coordinates
#' This function does not account for the possibility of points crossing the prime meridian and in cases where this occurs lines will cut across the entire plot
#' @examples
#' w<-MSTDist(longs=c(23,78,-23,56),lats=c(21,4,55,-3))
#' PlotMST(MSTCalc=w)
#' @importFrom graphics par
#' @importFrom graphics plot
#' @importFrom graphics lines
#' @export
PlotMST<-function(MSTCalc,color="black",symbol=16,xlimit="NA",ylimit="NA"){
  xrange<-c(min(MSTCalc$Longitude,na.rm=TRUE),max(MSTCalc$Longitude,na.rm=TRUE))
  yrange<-c(min(MSTCalc$Latitude,na.rm=TRUE),max(MSTCalc$Latitude,na.rm=TRUE))
  nCoords<-length(MSTCalc$MST_pt_connects)
  SkipC<-seq(1,nCoords,2)
  par(tck=0.02,mgp=c(1.7,0.3,0))
  if(xlimit=="NA" | ylimit=="NA"){
    plot(MSTCalc$Longitude,MSTCalc$Latitude,type="p",pch=symbol,col=color,xlim=c(xrange[1],xrange[2]),ylim=c(yrange[1],yrange[2]),xlab="Longitude",ylab="Latitude")
  }
  else{
    plot(MSTCalc$Longitude,MSTCalc$Latitude,type="p",pch=symbol,col=color,xlim=xlimit,ylim=ylimit,xlab="Longitude",ylab="Latitude")
  }
  for(i in SkipC){
    j<-i+1
    lines(c(MSTCalc$Longitude[MSTCalc$MST_pt_connects[i]],MSTCalc$Longitude[MSTCalc$MST_pt_connects[j]]),c(MSTCalc$Latitude[MSTCalc$MST_pt_connects[i]],MSTCalc$Latitude[MSTCalc$MST_pt_connects[j]]),col=color)
  }
}
