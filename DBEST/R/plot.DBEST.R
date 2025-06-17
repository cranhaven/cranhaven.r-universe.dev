plot.DBEST <-
  function(x, figure=1, ...) {
    
    if(figure==1) {
      
      par(mfrow=c(2,1))
      par(mar=c(0,5,3,3))
      plot(x$Data, main="DBEST fig.1", ylab="Data", xaxt='n', yaxt='n',cex.lab=0.8)
      axis(side=4)
      
      fit_zoo <- as.zoo(ts(x$Fit, start=as.numeric(time(x$Data)[1]), frequency=frequency(x$Data)))
      lines(fit_zoo, lwd=2, col="blue")
      
      if(exists("Start", where=x)){
        if(length(x$Start)>0) {
          for(t in 1:length(x$Start)) {
            abline(v = index(fit_zoo)[x$Start[t]], lty = 2, col="gray")
            
            segm <- fit_zoo[x$Start[t]:x$End[t]]
            if(x$ChangeType[t] == 1) {
              lines(segm, lwd=2, col="red")
            } else {
              lines(segm, lwd=2, col="orange")
              
            }
          }
          
        }
      }
      
      par(mar=c(8,5,0,3))
      f_local_zoo <- as.zoo(ts(x$f_local, start=as.numeric(time(x$Data)[1]), frequency=frequency(x$Data)))
      plot(f_local_zoo, main=NULL, type = "n", yaxt = "n", bty="n", ylab = "", xlab="")
      
      par(new=T)
      barplot(f_local_zoo, main=NULL, ylab="Trend Local Change", xaxt='n', xlab="",cex.lab=0.8, border='red', col='red', space=100)
      box()
      
    } 
    
    if(figure==2 & exists("Seasonal", where=x)) {
      
      par(mfrow=c(4,1))
      
      par(mar=c(0,5,3,3))
      plot(x$Data, main="DBEST fig.2",ylab="Data", xaxt='n', yaxt='n')
      axis(side=4)
      
      par(mar=c(0,5,0,3))
      trend_zoo <- as.zoo(ts(x$Trend, start=as.Date(time(x$Data)[1]), frequency=frequency(x$Data)))
      plot(trend_zoo, type="l", ylab="Trend", xaxt='n', main=NULL)
      
      par(mar=c(0,5,0,3))
      seasonal_zoo <- as.zoo(ts(x$Seasonal, start=as.Date(time(x$Data)[1]), frequency=frequency(x$Data)))
      plot(seasonal_zoo, ylab="Seasonal", xaxt='n', yaxt='n', main=NULL)
      axis(side=4)
      
      par(mar=c(3,5,0,3))
      remainder_zoo <- as.zoo(ts(x$Remainder, start=time(x$Data)[1], frequency=frequency(x$Data)))
      plot(remainder_zoo, main=NULL, type = "n", yaxt = "n", bty="n", ylab = "")
      
      par(new=T)
      barplot(remainder_zoo, ylab="Remainder", main=NULL, xaxt='n', border='#808080', col='#808080', space=100)
      box()
      
    }
  }
