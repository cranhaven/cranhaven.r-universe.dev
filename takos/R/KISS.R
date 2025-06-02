#' Title Kissinger
#' @description performs analysis of the thermograms using Kissinger method to calculate the activation energy (Ea)
#' @param mat matrix of the all the thermograms checked using the functiom mat.check
#' @return models "mod", datable "xy" for plot, "Ea" list of value, datatable "DT" built with the values of mat according to the specified degrees
#' @references 1. Avrami M. Kinetics of Phase Change. I General Theory. J Chem Phys. 1939;7(12):1103-1112. doi:10.1063/1.1750380.
#' 2. Kissinger HE. Reaction Kinetics in Differential Thermal Analysis. Anal Chem. 1957;29(11):1702-1706. doi:10.1021/ac60131a045.
#' @export
#' @examples \donttest{
#' require(data.table)
#' require(MASS)
#' rates=c(0.5,1,2,5,10,20,50)
#' a<-lapply(rates, function(x) JMA(A=exp(35),Ea=120000,T0=0,T.end=300,q=x,npoints=5000,n=2))
#' a<-lapply(seq(1,length(a)), function(x) data.table(a[[x]]$time.s,a[[x]]$T.C,
#' a[[x]]$dadT, rates[[x]]))
#' lapply(seq(1,length(a)), function(x) setnames(a[[x]],
#' c("time.seconds","temperature.s","heat.flow","rates") ) )
#' ar<-testMat(a)
#' kiss<-Kiss(ar)
#' }

Kiss <- function(mat) {
  time.minutes.check <- time.minutes <- id_cycle <- rit <-time.minutes.zero<-temperature.s<-heat.flow<- NULL

  mat.zero <- mat[mat$ri > 0][, time.minutes.check := (time.minutes.zero - min(time.minutes.zero)), by = id_cycle][]
  T.peak.max <- (mat.zero[, temperature.s[which.max(heat.flow)], by = id_cycle])
  DT <- mat.zero[, .SD[which.max(heat.flow)],by=id_cycle]

  #-------------
  x <- 1000/DT$temperature.s.K
  y <- log(DT$rate/(DT$temperature.s.K)^2)
  #-------------

  xy <- data.table(x, y, DT$rate)
  setnames(xy, c("x", "y", "rate"))
  xy <- xy[is.finite(rowSums(xy)), ]
  mod=lm(xy$y~xy$x)
  Ea=-mod$coeff[[2]]* 8.314
  my.list <- list("mod" = mod, "xy" = xy,"mod" = mod,"Ea"=Ea, "DT"=DT)
  print(summaryTableKiss(my.list))
  return(my.list)
}
