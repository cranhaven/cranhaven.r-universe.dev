#' Title Avrami
#' @description performs analysis of the thermograms using the avrami method
#' @param mat matrix of the all the thermograms checked using the functiom mat.check
#' @return models "mod", datable "xy" for plot
#' @export
#' @import minpack.lm data.table
#' @references 1. Avrami M. Kinetics of Phase Change. I General Theory. J Chem Phys. 1939;7(12):1103-1112. doi:10.1063/1.1750380.
#' @examples  \donttest{
#' require(data.table)
#' require(MASS)
#' rates=c(0.5,1,2,5,10,20,50)
#' a<-lapply(rates, function(x) JMA(A=exp(35),Ea=120000,T0=0,T.end=300,q=x,npoints=5000,n=2))
#' a<-lapply(seq(1,length(a)), function(x) data.table(a[[x]]$time.s,a[[x]]$T.C,
#' a[[x]]$dadT, rates[[x]]))
#' lapply(seq(1,length(a)), function(x) setnames(a[[x]],
#' c("time.seconds","temperature.s","heat.flow","rates") ) )
#' ar<-testMat(a)
#' avr<-avrami(ar)
#' }

avrami <- function(mat) {
  time.minutes.check <- time.minutes <- id_cycle <- rit <-rate<-time.minutes.zero <- NULL

  #require(minpack.lm)
  mat.zero <- mat[mat$ri > 0][, time.minutes.check := (time.minutes.zero - min(time.minutes.zero)), by = id_cycle][]
  Xc <- mat.zero$ri
  t.m <- mat.zero$time.minutes.check
  x <- t.m
  y <- Xc
  xy <- data.table(x, y, mat.zero$rate)
  setnames(xy, c("x", "y", "rate"))
  xy <- xy[is.finite(rowSums(xy)), ]
  formulas <- list(y ~ (1 - exp(-Z * (x^n))))
  mod <- xy[, lapply(formulas, function(x) list(nlsLM(x, data = .SD, start = list(Z = 0.001, n = 1)))), by = rate]
  my.list <- list("mod" = mod, "xy" = xy)

  print(summaryTableA(my.list))
  return(my.list)
}
