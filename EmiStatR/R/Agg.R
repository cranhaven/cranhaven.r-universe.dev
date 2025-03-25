# aggregation of data.frame function
# author: J.A. Torres-Matallana
# organization: LIST
# date: 15.07.2015 - 19.07.2016

# data     <- P1
# nameData <- deparse(substitute(P1))
# delta    <- 1
# func     <- "sum"

# data     <- wlt_obs
# nameData <- "wlt_obs"
# delta    <- 1
# func     <- "mean"

Agg <- function(data, nameData, delta, func, namePlot){
  # data <- var; nameData <- var.name; delta <- 60; func <- "mean"; namePlot <- "hourly"
  #---------------------------------------------------------------------------------------------------------
  # aggregating to 10, 30, 60 min resolution
  #---------------------------------------------------------------------------------------------------------
  tt <- as.POSIXct(data[,1], tz="UTC")
  
  # delta min
  dt <- 60/1*delta # 60_s/1_min * delta_min = dt_s
  bucket = (tt) - as.numeric(tt) %% dt
  namePlot <- paste(namePlot, "(res =", delta, "min)", sep=" ")
  
  if(nameData == "P1"){
    P1 <- data
    head(P1)
    ts <- aggregate(P1$rainfall, list(bucket), func)
    ts[,3] <- NA
    colnames(ts) <- c("time", "rainfall", "intensity")
    #length(P1$Rainfall)
    #length(ts$Rainfall)
    # head(ts)
    
    par(mfrow = c(2, 1))
    par(mar = rep(2, 4)) #------------------------------------------ added after MC set-up
    plot(P1$rainfall, type="l", main=namePlot) #------------------------------------------ commented after MC set-up
    plot(ts$rainfall, type="l")
      
    P1 <- ts
    # head(P1)
    # save(P1, file="P1.RData")
    
    return(P1)
  }else{
    obs <- data
    head(obs)
    ts <- aggregate(data$value, list(bucket), func)
    head(ts)
    head(obs)
    length(obs$value)
    length(ts$x)
    
    # commented out to avoid creation of local file (pdf plot)
    #pdf(paste(namePlot, ".pdf", sep=""), pointsize=10)
    #par(mfrow = c(2,1))
    #par(cex.lab=1, cex.axis=1., cex.main = 1.5)
    #plot(obs$time,obs$value, type="l", main="Original time series", xlab = "Time", ylab = nameData)#------ commented after MC set-up
    #plot(ts[,1],ts$x, type="l", main=namePlot, xlab = "Time", ylab = nameData)
    #dev.off()
    
    colnames(ts) <- c("time", "value")
    obs <- ts
    #save(obs, file="obs.RData")
    
    return(obs)
  }
}