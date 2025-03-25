# delay function for time series
# author: J.A. Torres-Matallana
# organization: LIST, WUR
# date: 17.08.2017 - 17.08.2017

Delay <- function(P1, x){
  if(x > 0){
    a <- IsReg(P1, "%Y-%m-%d %H:%M:%S", "UTC")
    a[[1]]
    xts <- a[[2]]
    
    ini     <- index(xts)[1]
    ini.new <- index(xts)[1+x]
    
    deltat <- as.numeric(difftime(index(xts)[2], index(xts)[1], units = "mins"))
    time.new <- seq(ini.new, by = deltat*60, length.out = nrow(xts))
    xts.new <- as.xts(coredata(xts), order.by = time.new)
    
    P1.new <- cbind.data.frame(index(xts.new), coredata(xts.new))
    colnames(P1.new) <- colnames(P1)
  }else{
    P1.new <- P1
  }
  
  # x11()
  # par(mfrow = c(2, 1))
  # plot(P1, typ = "l")
  # plot(P1.new, typ = "l", col = "blue")
  
  return(P1.new)
}