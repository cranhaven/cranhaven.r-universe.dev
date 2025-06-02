#' title Vyazovkin
#' @description performs analysis of the thermograms using Vyazovkin isoconversional method to calculate the activation energy (Ea)
#' @param bet rate
#' @param T temperature
#' @param Ea estimated Ea to use as a first guess for the iterative process
#' @references VYAZOVKIN, S. Advanced isoconversional method. Journal of thermal analysis, 1997, 49.3: 1493-1499.
#'
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
#' as<-select_degree(ar)
#' vy<-as[, optimize(function(x) VY(temperature.s.K,rate,x), lower=50,upper=250),by=rit]
#' }


VY <- function(T, bet, Ea) {
  n=length(T)
  integrand <- function(X) {
    exp(-Ea * 1000 / (8.314462 * X))
  }
  my.phi <- array(list(), c(n, n))
  for (intI in 1:n)
  {
    for (intJ in 1:n)
    {
      if (intI != intJ) {
        phi.num <- (integrate(integrand, lower = 273.15, upper = (T[intI])))
        phi.den <- (integrate(integrand, lower = 273.15, upper = (T[intJ])))
        a <- (phi.num$value) * bet[intJ]
        b <- (phi.den$value) * bet[intI]
        phi <- a / b
        my.phi[intI, intJ] <- phi
        #y.phi <- sum(unlist(my.phi))
      }
    }
  }
  my.phi <- sum(unlist(my.phi))
  return(my.phi)
}








