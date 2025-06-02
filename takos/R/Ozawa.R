#'Title Ozawa model crystallization
#' @description performs analysis of the thermograms using Ozawa method
#' @param mat matrix of the all the thermograms checked using the functiom mat.check
#' @param n.step number of steps for selecting temperature ranges
#' @param spks id of the peaks selected for applying the method
#' @param eps tollerance for the selection process
#' @return models "mod", datable "xy" for plot, "Ea" list of value, datatable "DT" built with the values of mat according to the specified degrees
#' @references 1. Ozawa T. Kinetics of non-isothermal crystallization. Polymer (Guildf). 1971;12(3):150-158. doi:10.1016/0032-3861(71)90041-3.
#' @export
#' @import data.table pracma
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
#' oz<-OZ(ar)
#' }

OZ <- function(mat, n.step=1, spks=1, eps=0.001) {
  time.minutes.check <- time.minutes <- id_cycle <- rit <-time.minutes.zero<-temperature.s<-heat.flow<-T.deg<- NULL

  mat.zero <- mat[mat$ri > 0][, time.minutes.check := (time.minutes.zero - min(time.minutes.zero)), by = id_cycle][]

  T.peak.max <- (mat.zero[, temperature.s[which.max(heat.flow)], by = id_cycle])
  T.step <- ceil(((max(T.peak.max$V1) - min(T.peak.max$V1)) / nrow(T.peak.max))) / n.step
  T.range <- seq((floor(min(T.peak.max$V1))), (ceil(max(T.peak.max$V1)) + T.step), by = T.step)


  DT <- lapply(T.range, function(x) mat.zero[, .SD[which.min(abs(temperature.s - x))], by = list(id_cycle)])

  for (i in 1:length(T.range))
  {
    DT[[i]]$T.deg <- T.range[[i]]
  }

  DT <- rbindlist(DT)

  #-------------
  Xc <- na.omit(DT$ri)
  logPhi <- log10(na.omit(DT$rate))
  x <- logPhi
  y <- log10(-log((1 - Xc)))

  #-------------

  xy <- data.table(x, y, DT$id_cycle, DT$T.deg)
  setnames(xy, c("x", "y", "id_cycle", "T.deg"))
  xy <- xy[is.finite(rowSums(xy)), ]
  formulas <- list(y ~ x)
  mod <- xy[, lapply(formulas, function(x) list(lm(x, data = .SD))), by = T.deg]
  m <- lapply(mod$V1, function(x) -x$coeff[[2]])
  my.list <- list("mod" = mod, "xy" = xy, "m" = m)

  print(summaryTableOz(my.list))
  return(my.list)
}
