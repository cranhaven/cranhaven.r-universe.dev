\name{chrono_Gauss}
\alias{chrono_Gauss}
\title{
Bayesian chronologies of Gaussian dates
}
\description{
Bayesian modeling for combining Gaussian dates. These dates are assumed to be contemporaneous of the event date. The posterior distribution is sampled by a MCMC algorithm as well as those of all parameters of the Bayesian model. 
}
\usage{
chrono_Gauss(M, s, refYear=NULL, studyPeriodMin, studyPeriodMax, 
        numberChains = 2, numberAdapt = 10000, numberUpdate = 10000, 
        variable.names = c("theta"), numberSample = 50000, thin = 10)
}
\arguments{
  \item{M}{vector of measurement}
  \item{s}{vector of measurement errors}
  \item{refYear}{vector of year of reference for ages for coversion into calendar dates}
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
\examples{
  ### simulated data (see examples(chronoEvent_Gauss))
  
  # Number of events 
  Nevt  = 3 
  # number of dates by events  
  measurementsPerEvent = c(2,3,2)
  # positions 
  pos = 1 + c(0, cumsum(measurementsPerEvent) ) 
  
  # simulation of data    
  theta.evt = seq(1,10, length.out= Nevt)

  theta = NULL 
  for(i in 1:Nevt ){
    theta = c(theta, rep(theta.evt[i],measurementsPerEvent[i])) 
    } 
    
  s = seq(1,1, length.out= sum(measurementsPerEvent)) 
  
  M=NULL 
  for( i in 1:sum(measurementsPerEvent)){
    M= c(M, rnorm(1, theta[i], s[i] ))
    }  
  
  MCMCSample = chrono_Gauss(M, s, studyPeriodMin=-10, studyPeriodMax=30)
  plot(MCMCSample)
}
\keyword{ Chronology }
\keyword{ Chronology of dates }
\keyword{ Gaussian dates }