\name{chronoEvents_Gauss}
\alias{chronoEvents_Gauss}
\title{
Bayesian chronologies of Gaussian dates using the Event Model
}
\description{
Bayesian modeling for combining Gaussian dates. These dates are assumed to be contemporaneous of the event date. The posterior distribution of the event date is sampled by MCMC algorithm as well as those of all parameters of the Bayesian model as described in Lanos & Philippe (2017). 
}
\usage{
chronoEvents_Gauss(M, s, refYear=NULL, measurementsPerEvent, studyPeriodMin, 
      studyPeriodMax, numberChains = 2, numberAdapt = 10000, numberUpdate = 10000, 
      variable.names = c("theta"), numberSample = 50000, thin = 10)
}
\arguments{
  \item{M}{vector of measurement}
  \item{s}{vector of measurement errors}
  \item{refYear}{vector of year of reference for ages for coversion into calendar dates}
  \item{measurementsPerEvent}{vector containing the number of measurements associated with the first event, then the second ...}
  \item{studyPeriodMin}{numerical value corresponding to the start of the study period in BC/AD format}
  \item{studyPeriodMax}{numerical value corresponding to the end of the study period in BC/AD format}
  \item{numberChains}{number of Markov chains simulated}
  \item{numberAdapt}{number of iterations in the Adapt period of the MCMC algorithm}
  \item{numberUpdate}{number of iterations in the Update period of the MCMC algorithm}
  \item{variable.names}{names of the variables whose Markov chains are kept}
  \item{numberSample}{number of iterations in the Acquire period of the MCMC algorithm}
  \item{thin}{step between consecutive iterations finally kept}
}
\value{
This function returns a Markov chain of the posterior distribution. The MCMC chain is in date format BC/AD, that is the reference year is 0. Only values for the variables defined by 'variable.names' are given.}
\author{
Anne Philippe & Marie-Anne Vibet
}
\references{
P. Lanos and A. Philippe. Hierarchical Bayesian Modeling for Combining Dates in Archaeological Context. Journal de la SFdS, Vol. 158 (2) pp 72-88 2017.
}
\examples{
  ### simulated data 
  
  # Number of events 
  Nevt  = 3 
  # number of dates by events  
  measurementsPerEvent = c(2,3,2)
  # positions 
  pos = 1 + c(0, cumsum(measurementsPerEvent) ) 
  
  # simulation of data    
  theta.evt = seq(1,10, length.out= Nevt)
  theta.evt[3] <-theta.evt[3] - 3 # stratigraphic inversion 

  theta = NULL 
  for(i in 1:Nevt ){
    theta = c(theta, rep(theta.evt[i],measurementsPerEvent[i])) 
    } 
    
  s = seq(1,1, length.out= sum(measurementsPerEvent)) 
  
  M=NULL 
  for( i in 1:sum(measurementsPerEvent)){
    M= c(M, rnorm(1, theta[i], s[i] ))
    }  
  
  sO2 = 1:Nevt 
  for (i in  1:Nevt) {
  sO2[i]= 1/mean( 1/(s[pos[i]:(pos[i+1] -1)])^2 ) 
  }
  
  MCMCSample = chronoEvents_Gauss( M=M, s=s, measurementsPerEvent=measurementsPerEvent, 
  studyPeriodMin=-10, studyPeriodMax=30)
  plot(MCMCSample)
}
\keyword{ Chronology }
\keyword{ Event Model }
\keyword{ Gaussian dates }