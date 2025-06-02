#' Title Johnson-Mehl-Avrami (JMA)
#' @description simulate a thermogram using JMA theory
#' @param A pre exponential parameters (1/s)
#' @param Ea Activation energy (J/mol)
#' @param q = rate of analyis (K/min)
#' @param n numerical parameter required by JMA model
#' @param T0 = starting temperature of the simulated thermogram expressed in K
#' @param T.end  = ending temperature of the simulated thermogram expressed in K
#' @param npoints  desired number of points of the simulate thermogram
#' @return \itemize{\item T.C = temperature in Celsius
#' \item \eqn{fi=d\alpha/dt * q}
#' \item \eqn{\alpha}
#' \item time.s = time in second
#' \item \eqn{d\alpha dT} }
#' @export
#' @references  1. Vyazovkin S, Chrissafis K, Di Lorenzo ML, et al. ICTAC Kinetics Committee recommendations for collecting experimental thermal analysis data for kinetic computations. Thermochim Acta. 2014;590:1-23. doi:10.1016/j.tca.2014.05.036.
#' @examples
#' data <- JMA(A = exp(35),Ea = 120000,q = 50,T0 = -100,T.end = 300,npoints=898,n=2)
#'
#' require(data.table)
#' #choose the rates for the simulation of the thermograms
#' rates=c(0.5,1,2,5,10,20,50)
#' #first serie of thermograms for all the chosen rate
#' a<-lapply(rates, function(x) JMA(A=exp(35),Ea=120000,T0=0,T.end=300,q=x,npoints=5000,n=2))
#' #setup column names
#' a<-lapply(seq(1,length(a)), function(x) data.table(a[[x]]$time.s,
#' a[[x]]$T.C, a[[x]]$dadT, rates[[x]]))
#' lapply(seq(1,length(a)), function(x) setnames(a[[x]],
#' c("time.seconds","temperature.s","heat.flow","rates") ) )
#' #create a plot using the function thermo
#' amaxH <- max(sapply(a, function(x) max(x$heat.flow))) # calculate the max
#' plot(c(0,300),c(0,amaxH),mytitle="dataset A 120/60 0.66/0.33",
#' ylab="ExothermicHeatFlow", xlab="Temperature")
#' lapply(a, function(x) lines(x$temperature.s,x$heat.flow,lwd=3))



JMA <- function (A=exp(35),Ea=120000,q=50,T0=-100,T.end=300,npoints=898,n=2) {
  R=8.314
  Ts=273.15+T0
  time.e=(T.end-T0)/(q/60)
  time.s=seq(0,time.e, length.out=npoints)
  T=Ts+(time.s*(q/60))
  K=A*exp((-1)*Ea/(R*T))
  T.C=T-273.15
  dT=diff(T)
  dt<-diff(c(0,time.s))
  Kint<-cumsum((c(0,K)+K)[seq(0,length(K)-1)]*(diff(c(0,time.s))/2))
  alpha=1-exp((-1)*Kint^n)
  da=diff(alpha)
  dadT=c(0,da/dT)
  fi=dadT*q/60
  alfa=alpha*100
  my.list <- list("T.C" = T.C, "fi"=fi, "alfa"=alfa, "time.s"=time.s, "dadT"=dadT)
  return(my.list)
}

