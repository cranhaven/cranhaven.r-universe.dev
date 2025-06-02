
#' Title Mo model
#' @description performs analysis of the thermograms using Mo method
#' @param mat matrix of the all the thermograms checked using the functiom mat.check
#' @param degree selected degrees of  cristallinity for performing the analysis
#' @return models "mod", datable "xy" for plot, "Ea" list of value, datatable "DT" built with the values of mat according to the specified degrees
#' @references  Liu T, Mo Z, Wang S, Zhang H. Nonisothermal melt and cold crystallization kinetics of poly(aryl ether ether ketone ketone). Polym Eng Sci. 1997;37(3):568-575. doi:10.1002/pen.11700.
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
#' mo<-MO(ar)
#' }

MO <- function(mat, degree=seq(0.2, 0.8, by = 0.2)) {
  time.minutes.check <- time.minutes <-time.minutes.zero<- id_cycle <- rit <- NULL

  mat.zero <- mat[mat$ri > 0][, time.minutes.check := (time.minutes.zero - min(time.minutes.zero)), by = id_cycle][]

  DT <- lapply(degree, function(x) mat.zero[, .SD[which.min(abs(ri - x))], by = list(id_cycle)])

  for (i in 1:length(degree))
  {
    DT[[i]]$rit <- degree[[i]]
  }

  DT <- rbindlist(DT)

  #-------------
  x <- log10(na.omit(DT$time.minutes.check))
  y <- log10(DT$rate)
  #-------------

  xy <- data.table(x, y, DT$ri, DT$rate, DT$rit)
  setnames(xy, c("x", "y", "ri", "rate", "rit"))
  xy <- xy[is.finite(rowSums(xy)), ]
  formulas <- list(y ~ x)
  mod <- xy[, lapply(formulas, function(x) list(lm(x, data = .SD))), by = rit]
  my.list <- list("mod" = mod, "xy" = xy)

  print(summaryTableMo(my.list))
  return(my.list)
}
