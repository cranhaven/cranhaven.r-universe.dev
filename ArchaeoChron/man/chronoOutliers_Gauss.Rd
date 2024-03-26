\name{chronoOutliers_Gauss}
\alias{chronoOutliers_Gauss}
\title{
Bayesian chronologies of Gaussian dates using the outlier modelling of Oxcal software
}
\description{
Bayesian modeling for combining Gaussian dates. These dates are assumed to be contemporaneous of the event date. The posterior distribution is sampled by a MCMC algorithm as well as those of all parameters of the Bayesian model. 
}
\usage{
chronoOutliers_Gauss(M, s, refYear=NULL, outliersIndivVariance, outliersBernouilliProba, 
        studyPeriodMin, studyPeriodMax, 
        numberChains = 2, numberAdapt = 10000, numberUpdate = 10000, 
        variable.names = c("theta"), numberSample = 50000, thin = 10)
}
\arguments{
  \item{M}{vector of measurement}
  \item{s}{vector of measurement errors}
  \item{refYear}{vector of year of reference for ages for coversion into calendar dates}
  \item{outliersIndivVariance}{vector of individual variance for delta[i]}
  \item{outliersBernouilliProba}{vector of Bernouilli probability for each date. Reflects a prior assumption that the date is an outlier. }  
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
Bronk Ramsey C., Dealing with outliers and offsets in Radiocarbon dating, Radiocarbon, 2009, 51:1023-45.
}
\examples{
  ### simulated data (see examples(chronoEvent_Gauss))
  
  # Number of event 
  Nevt  = 3 
  # number of dates by events  
  measurementsPerEvent = c(2,3,2)
  # positions 
  pos = 1 + c(0, cumsum(measurementsPerEvent) ) 
  
  # simulation of data    
  theta.evt = seq(1,10, length.out= Nevt)
  theta.evt[3] <- theta.evt[3] - 3 # stratigraphic inversion 

  theta = NULL 
  for(i in 1:Nevt ){
    theta = c(theta, rep(theta.evt[i],measurementsPerEvent[i])) 
    } 
    
  s = seq(1,1, length.out= sum(measurementsPerEvent)) 
  
  M=NULL 
  for( i in 1:sum(measurementsPerEvent)){
    M= c(M, rnorm(1, theta[i], s[i] ))
    }  
  
  MCMCSample = chronoOutliers_Gauss(M, s, outliersIndivVariance = rep(5,7), 
  outliersBernouilliProba=rep(0.2,7), studyPeriodMin=-10, studyPeriodMax=30, 
  numberAdapt = 1000, numberUpdate = 1000, numberSample = 5000)
  plot(MCMCSample)
}
\keyword{ Chronology }

\keyword{ Oxcal }
\keyword{ Gaussian dates }