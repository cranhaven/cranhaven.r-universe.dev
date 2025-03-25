# CInp2TS: Constant Input to Time Series
# author: J.A. Torres-Matallana, U. Leopold
# organization: Luxembourg Institute of Science and technology (LIST)
#               Wagenigen university and Research Centre (WUR)   
# date: 24.08.2016 - 02.05.2019

CInp2TS <- function(cinp, prec, cinp.daily.file, cinp.weekly, cinp.seasonal){
#   cinp            <- 611 # Goesdorf 2010
#   p.file          <- "/home/atorres/EmiStatR/EmiStatR_120_1/inst/shiny/EmiStatR_input/input/P1.RData"
#   cinp.daily.file <- "/home/atorres/EmiStatR/EmiStatR_120_1/inst/shiny/EmiStatR_inputCSO/inputCSO/DWF_ATV-A134.csv"
#   cinp.weekly     <- list(mon=1, tue=.83, wed=.83, thu=.83, fri=1, sat=1.25, sun=1.25)  # factor of change, average = 1.00
#   cinp.seasonal   <- list(jan=.79, feb=.79, mar=1.15, apr=1.15, may=1.15, jun=1.15, 
#                      jul=1.15, aug=1.15, sep=1.15, oct=1.15, nov=.79, dec=.79) # factor of change, average = 1.000
#   
#   
#   ## loading precipitation time series
#   load(p.file)
#   head(P1); tail(P1,10)
#   
#   library(EmiStatR)
#   data("Esch_Sure2010")
#   P1 <- Esch_Sure2010
#   head(P1); tail(P1)
#   
#   prec <- P1
# #   class(P1[4465,1])
# #   df.new <- data.frame(as.POSIXct("2016-02-01 00:00:00"))
# #   df.new$P <- 0
# #   colnames(df.new) <- c("time", "P [mm]")
# #   P1 <- rbind(P1, df.new)
# #   save(P1, file="P1.RData")
# #   getwd()
  
  ## ===================================================================================================
  ## reading precipitation time series
  P1 <- prec
  a        <- IsReg(data = P1, format="%Y-%m-%d %H:%M:%S", tz="UTC")
  
  # if(a[[1]] != "_TSregular") stop("precipitation time series is not regular")
  
  p.ts     <- a[[2]][,1]
  p.deltat <- as.numeric(difftime(P1[2,1], P1[1,1], units = "min"))
  colnames(p.ts) <- "p"
  
  # plot.xts(p.ts)

  ## reading daily population equivalent factors
  daily.data   <- cinp.daily.file
  
  a            <- IsReg(data = daily.data, format="%H:%M:%S", tz="UTC")
  # a[1]
  daily.ts     <- a[[2]]
  colnames(daily.ts) <- "factor"
  # plot.xts(daily.ts)

  ## computing daily cinp
  daily.ts$cinp  <- cinp*daily.ts$factor 
  # plot.xts(daily.ts$cinp)
  
  ## creating cinp time series of precipitation length
  daily.deltat <- deltat(daily.ts) # in minutes
  
  p.end <- format(end(p.ts), format = "%Y-%m-%d")
  p.new.end <- end(p.ts) + 86400
  format(end(p.ts), format = "%H:%M:%S")
  format(end(daily.ts), format = "%H:%M:%S")

  # library(zoo)
  g  <- seq(as.POSIXct(P1[1,1]), p.new.end, by = paste(daily.deltat, "min"))
  g[end(g)[1]]
  g1 <- seq(as.POSIXct(P1[1,1]), length.out = length(daily.data[,2]), by = paste(daily.deltat, "min"))  
  
  g11 <- rep(daily.data[,2], ceiling(length(g)/length(g1)))
  #diff <- length(g11) -length(g)
  #g11 <- g11[1:(length(g11)-diff)]
  
  g  <- seq(as.POSIXct(P1[1,1]), length.out = length(g11), by = paste(daily.deltat, "min"))

  cinp.ts <- xts(x = g11, order.by = g)
  colnames(cinp.ts) <- "factor"
  
  cinp.ts$cinp <-   cinp*cinp.ts$factor
  # plot.xts(cinp.ts$cinp)
  # plot.xts(cinp.ts$cinp[1:(5*12)])

  cinp.ts.deltap <- seq(as.POSIXct(P1[1,1]), end(p.ts), by = paste(p.deltat, "min"))
  cinp.ts.deltap <- na.approx(cinp.ts, xout = cinp.ts.deltap)
  # plot.xts(cinp.ts.deltap$cinp)
  #  plot.xts(cinp.ts.deltap$cinp[1:(50*12)])
  
  cinp.week <- matrix(data = NA, nrow = length(cinp.ts.deltap$cinp), ncol = 1, byrow = T)
  cinp.season <- cinp.week
  
  for(i in 1:length(cinp.ts.deltap$cinp)){
    ifelse(.indexwday(cinp.ts.deltap[i,1]) == 0, 
           cinp.week[i] <- coredata(cinp.ts.deltap$cinp[i])*cinp.weekly[[7]],
           cinp.week[i] <- coredata(cinp.ts.deltap$cinp[i])*cinp.weekly[[.indexwday(cinp.ts.deltap[i,1])]]
           )
  }
  
  cinp.ts.deltap$cinp.week <- cinp.week
  
  for(i in 1:(length(cinp.ts.deltap$cinp))){
      cinp.season[i] <- coredata(cinp.ts.deltap$cinp.week[i])*cinp.seasonal[[.indexmon(cinp.ts.deltap[i,1])+1]]
  }
  
  cinp.ts.deltap$cinp.season <- cinp.season
  cinp.ts.deltap <- cinp.ts.deltap[1:nrow(P1),]
  #head(cinp.ts.deltap); tail(cinp.ts.deltap,1000)

  # plot.xts(cinp.ts.deltap$cinp.season)
  # abline(h = mean(cinp.ts.deltap$cinp.season), col="red")
  # 
  #hist(log(cinp.ts.deltap$cinp.season))
  #plot.xts(cinp.ts.deltap$cinp.season[1:(5000)])

  # m     <- seq(as.POSIXct(P1[1,1]), length.out = 12, by = paste(1, "month"))
  # m.ts  <- xts(1:12, order.by=m)
  # .indexmon(m.ts) 
  # cinp.ts.deltap[2250:3000,]
  
  ## checking average cinp
  # cinp.mean <- mean(cinp.ts.deltap$cinp)
  # cinp.week.mean <- mean(cinp.ts.deltap$cinp.week)
  # cinp.season.mean <- mean(cinp.ts.deltap$cinp.season)
  
  cinp.ts.deltap.time <- index(cinp.ts.deltap)
  cinp.ts.deltap.data <- coredata(cinp.ts.deltap)

  return(list(time=cinp.ts.deltap.time, data=cinp.ts.deltap.data, xts=cinp.ts.deltap))
}