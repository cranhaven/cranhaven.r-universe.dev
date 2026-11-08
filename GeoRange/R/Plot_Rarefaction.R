#' Plots the measured value versus rarefied samples or percent error of estimates (PEE) of a rarefied samples for a geographic range measure
#' @param Mes1_AllTaxa - Vector list of measured values for multiple taxa, from the GeoRarefaction_MultiTaxa function, or PEE calculations for multiple taxa, from from PEE_MultiTaxa function
#' @param Mes2_AllTaxa - Vector list of measured values for multiple taxa, output from GeoRarefaction_MultiTaxa function
#' @param symbol - Symbol used for plotting, as per pch graphical parameter
#' @param measure - Specifies which measure to be plotted, 2=MST 3=CH 4=GCD 5=LatRg 6=LongRg 7=CellCount
#' @param SampSize - Specifies the index value of the rarefaction sample size from the PEE_AllTaxa parameter
#' @param color - Specifies the color of symbols being plotted
#' @return Returns a plot of measured geographic range values or PEE versus true value for a specific geographic range measure at varying sample sizes
#' @details For each taxon for a specific geographic range measure using 95% confindence intervals rather than standard deviations as the error bars
#' Measure paramter is the ordinal position of the measure of interest 2=MST,3=CH,4=GCD,5=LatRg,6=LongRg,7=CellCount
#' SampSize parameter indicates the index column position of steps size, default is first column (all points)
#' @examples
#' \dontrun{
#' data(BivalvePBDB)
#' BivalveMatrix<-CoordList_PBDB(BivalvePBDB)
#' BivalveGeo<-GeoRarefaction_MultiTaxa(nLocCut=20,OccMatrix=BivalveMatrix,TaxaStart=3,replacePts=TRUE)
#' BivalvePEE<-PEE_MultiTaxa(BivalveGeo)
#' Plot_Rarefaction(BivalvePEE,BivalveGeo,symbol=20,measure=2,SampSize=2)
#' }
#' @importFrom stats median
#' @importFrom graphics plot
#' @importFrom graphics arrows
#' @export
Plot_Rarefaction<-function(Mes1_AllTaxa,Mes2_AllTaxa,symbol=20,measure=2,SampSize=1,color="black"){
  nTaxa<-length(Mes1_AllTaxa)
  medMes<-c()
  TrueValue<-c()
  perc5<-c()
  perc95<-c()
  rangeMaxTemp<-c()
  rangeMinTemp<-c()
  Posperc5<-round(length(Mes1_AllTaxa[[1]][[2]][,1])*0.05,0)
  if(Posperc5==0){
    perc5<-1
  }
  Posperc95<-round(length(Mes1_AllTaxa[[1]][[2]][,1])*0.95,0)
  for(i in 1:nTaxa){
    TrueValue<-c(TrueValue,Mes2_AllTaxa[[i]][[measure+1]][1,1])
    medMes<-c(medMes,median(Mes1_AllTaxa[[i]][[measure+1]][,SampSize]))
    rangeMaxTemp<-c(rangeMaxTemp,max(Mes1_AllTaxa[[i]][[measure+1]][,SampSize],na.rm=TRUE))
    rangeMinTemp<-c(rangeMinTemp,min(Mes1_AllTaxa[[i]][[measure+1]][,SampSize],na.rm=TRUE))
    perc5<-c(perc5,sort(Mes1_AllTaxa[[i]][[measure+1]][,SampSize])[Posperc5])
    perc95<-c(perc95,sort(Mes1_AllTaxa[[i]][[measure+1]][,SampSize])[Posperc95])
  }
  rangeMax<-max(rangeMaxTemp)
  rangeMin<-min(rangeMinTemp)
  if(names(Mes1_AllTaxa[[1]])[2]=="MST_dif"){
    plot(TrueValue,medMes,ylim=range(c(round(rangeMin,-1)-10,round(rangeMax,-1)+10)),pch=symbol,xlab=paste(names(Mes2_AllTaxa[[1]][measure+1]),"True Value"),ylab="Percent Error of Estimate",main=paste(names(Mes1_AllTaxa[[1]][[measure]][SampSize]),"Points"),col=color)
  }
  else{
    plot(TrueValue,medMes,ylim=range(c(round(rangeMin,-1)-10,round(rangeMax,-1)+10)),pch=symbol,xlab=paste(names(Mes2_AllTaxa[[1]][measure+1]),"True Value"),ylab=paste(names(Mes2_AllTaxa[[1]][measure+1]),"Measured Value"),main=paste(names(Mes1_AllTaxa[[1]][[measure]][SampSize]),"Points"),col=color)
  }
  arrows(x0=TrueValue,y0=medMes,x1=TrueValue,y1=perc5,length=0.05, angle=90, code=2,col=color)
  arrows(x0=TrueValue, y0=medMes,x1=TrueValue,y1=perc95,length=0.05, angle=90, code=2,col=color)
}
