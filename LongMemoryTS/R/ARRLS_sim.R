
########################################################
## ARLS-model
########################################################


#' @title Simulation of Autoregressive Random Level Shift processes.
#' @description Simulation of a AR-RLS process as discussed in and Xu and Perron (2014).
#' @details
#' add details here
#' @param T length of the desired series.
#' @param phi autoregressive parameter that determines the persistence of the shifts.
#'        For \code{phi=1} the process is a "stationary RLS" and for \code{phi=0} the process
#'        is a non stationary RLS. 
#' @param sig.shifts standard deviation of the shifts.
#' @param prob shift probability. For rare shifts p*/T, where p* is the expected number of shifts in the sample.
#' @param sig.noise standard deviation of the noise component. Default is \code{sig.noise=0}.
#' @param const mean of the process. Default is \code{const=0}.
#' @param trend trend of the process. Default is \code{trend=0}.
#' @param burnin length of the burnin period used. Default is \code{burnin=100}.
#' @author Christian Leschinski
#' @examples ts.plot(ARRLS.sim(T=500,phi=0.5, sig.shift=1, prob=0.05), ylab=expression(X[t]))
#' @references  Xu, J. and Perron, P. (2014): Forecasting return volatility: Level shifts with 
#'              varying jump probability and mean reversion. International Journal of Forecasting,
#'              30, pp. 449-463.
#' @export

ARRLS.sim<-function(T,phi,sig.shifts,prob,sig.noise=0,const=0,trend=0,burnin=100){
# simulates ARLS model with constant and trend
# n is the number of observations to be simulated
# by default constant and trend are set to zero and burnin period is 100 observations
# sig.eps is the standard deviation of the jumps
# prob is the probability of a jump to occur in any given period
  jump<-rbinom((burnin+T),1,prob)
  eps<-rnorm((burnin+T),0,sig.shifts)
  noise<-rnorm((burnin+T),0,sig.noise)
  mu<-0
  for(i in 2:(burnin+T)){mu[i]<-(1-phi*jump[i])*mu[(i-1)]+jump[i]*(eps[i])}
  Yt<-const+trend*cumsum(jump)+mu+noise
  #list("mu"=mu[(burnin+1):(burnin+T)], "jump"=jump[(burnin+1):(burnin+T)], "Yt"=Yt[(burnin+1):(burnin+T)])
  Yt[(burnin+1):(burnin+T)]
}


#sim<-ARRLS.sim(300, phi=0.5, sig.shifts=1, prob=0.05, burnin=0)
#ts.plot(sim)
#length(sim)

#est.none<-ARLS.est(sim, type="none")$coefficients
#est.intercept<-ARLS.est(sim, type="intercept")$coefficients

#est.trend<-ARLS.est(sim, type="trend")$coefficients

#est.none
#est.intercept
#est.trend

