proc.cdf.2d <-function (filename, mz.cut = 5e-4, rt.cut = 50, mz.search.range =  2e-3, rt.search.range = 200, mz.search.step = 5e-4, rt.search.step = 50, intensity.limit.quantile = 0.1, bPlot = FALSE, transform.mz=FALSE, transform.mz.const=0.1) #, min.run=8, min.pres=0.3, tol=1e-5, baseline.correct=0, intensity.weighted=FALSE)
{
  startTime <- proc.time();
  
  #####
  
  a<-load.lcms(filename)
  if(is.null(a))
    return ("Matrix Input Wrong");
  summary(a);
  sel<-which(a$masses>0)
  
  maOri<-a$masses[sel]
  ma <- maOri
  laOri<-a$labels[sel]
  la<- laOri
  pointLength <- length(ma);
  intenOri <- a$intensi[sel]
  inten<-log10(1+a$intensi[sel])
  
  if(transform.mz)
  {
	ma<-tra.mz(ma, transform.mz.const)
	mz.cut=mz.cut/100 # we use mz=100 as the reference point for window size. the window size grows larger
	mz.search.range = mz.search.range/100
	mz.search.step=mz.search.step/100
  }
  
  if(bPlot)
  {
    cols<-rep("grey", length(sel))
    rgb.palette <- colorRampPalette(c("blue", "cyan", "green", "yellow", "orange"), space = "rgb",bias=0.5)
    all.col<-rgb.palette(100)
    for(i in 0:99) cols[inten>=quantile(inten, i/100) & inten<quantile(inten, (i+1)/100)]<-all.col[i+1]
  }
  
  o<-order(ma,la)
  ma<-ma[o]
  la<-la[o]
  inten<-inten[o]
  
  if(bPlot)
  {
    cols<-cols[o]
    plot(ma, la,col = cols, cex=.1, xlab="ma", ylab="la")
  }
  
  pointLength <- length(ma);
  pointEndVector = rep(pointLength,pointLength);
  pointStartVector = rep(1,pointLength);
  pointResult = rep(0,pointLength);
  
  tempCoef <- -log(rt.cut^0.5) - 0.5 * 2 * log(2 * 3.14159265);
  
  # call calculator
  if(intensity.limit.quantile<=1) 
  {
	inten.thres<-quantile(inten,intensity.limit.quantile)
	}else{
	inten.thres<-quantile(inten,1)*intensity.limit.quantile
	}
  
  finalData<-calculateDensity(ma,la,inten,pointEndVector,pointStartVector,pointResult,tempCoef,mz.cut,rt.cut,mz.search.range,rt.search.range,mz.search.step,rt.search.step,inten.thres,1)
  
  peakNum = length(finalData) / 7
  
  finalMatrix <- matrix(0, peakNum, 7)
  
  finalMatrix[,1] = finalData[1:peakNum]
  finalMatrix[,2] = finalData[(peakNum+1):(peakNum*2)]
  finalMatrix[,4] = finalData[(peakNum*2+1):(peakNum*3)]
  finalMatrix[,3] = finalData[(peakNum*3+1):(peakNum*4)]
  finalMatrix[,5] = finalData[(peakNum*4+1):(peakNum*5)]
  #finalMatrix[,6] = finalData[(peakNum*5+1):(peakNum*6)]
  #finalMatrix[,7] = finalData[(peakNum*6+1):(peakNum*7)]
  
  if(transform.mz)
  {
    finalMatrix[,1]<-tra.back.mz(finalMatrix[,1], transform.mz.const)
  }
  
  if(bPlot)
    points(finalMatrix[,1], finalMatrix[,2], col="black", pch=4)
  
  finalMatrix <- finalMatrix[order(finalMatrix[,1],finalMatrix[,2]),]
  
  duringTime <- proc.time() - startTime
  message(paste(names(duringTime), round(duringTime, 2), collapse = ", "))
  
  return(finalMatrix)
}

