bin_TempTS <- function(ts, res=8, verbose=FALSE){
  
  sm0 <- plyr::ddply(ts,c("date"),function(x)c(nrecTempTS=nrow(x[which(!is.na(x$Temperature) & !is.na(x$Depth)),]))) ## check if dates missing!
  if(any(sm0$nrecTempTS == 0)) warning('no Temperature/DepthTS available for dates:\n',paste(sm0$date[which(sm0$nrecTempTS == 0)],collapse=", "))
  if(!is.null(ts$Corrected.Depth)){
    warning('Corrected.Depth column found. This data will be used instead!')
    ts$Depth <- ts$Corrected.Depth
  }
  ts <- ts[which(!is.na(ts$Temperature) & !is.na(ts$Depth)),]
  
  sm <- plyr::ddply(ts,c('date'),function(x)c(nrecTempTS=nrow(x)))
  dates <- as.character(sm$date)
  fdates <- c()
  pdt.rec <- c()
  d <- dates[1]
  for(d in dates){
    if(verbose) cat(paste(d,"\n"))
    i <- which(ts$date == d)
    x <- ts[i,]
    
    ###################################################" round bin time series data in res m intervals
    depth.range <- range(x$Depth)
    depth <- x$Depth/res
    
    ii <- which((x$Depth%%res)/res >= 0.5)
    depth[ii] <- ceiling(depth[ii])*res
    
    ii <- which((x$Depth%%res)/res < 0.5)
    depth[ii] <- trunc(depth[ii])*res
    
    x$Depth <- depth
    unique_depths <- unique(depth)
    if(length(unique_depths) > 2){
      h <- hist(depth,breaks = unique_depths,plot=F)
      
      identifiers <- c('DeployID','Serial','Ptt')
      identifiers <- identifiers[which(identifiers %in% names(ts))]
      add <- plyr::ddply(x[,which(names(x) %in% c(identifiers,'date','Depth','Temperature'))],
                         c(identifiers,'date','Depth'), function(x)
                           c(nrecs=nrow(x),
                             MeanTemp=mean(round(x$Temp,2)),
                             MinTemp=min(round(x$Temp,2)),
                             MaxTemp=max(round(x$Temp,2))))
      add$bin <- 1:nrow(add)
      pdt.rec <- rbind(pdt.rec,add)
    }
  }
  
  pdt.rec$MeanPDT <- rowMeans(cbind(pdt.rec$MaxTemp, pdt.rec$MinTemp))
  pdt.rec$date <- as.Date(pdt.rec$date)
  return(pdt.rec) 
}


