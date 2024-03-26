\name{eventModel_Gauss}
\alias{eventModel_Gauss}
\title{
Bayesian modeling for combining Gaussian dates using the Event Model
}
\description{
Bayesian modeling for combining Gaussian dates. These dates are assumed to be contemporaneous of the event date. The posterior distribution of the event date is sampled by MCMC algorithm as well as those of all parameters of the Bayesian model as described in Lanos & Philippe (2017). 
}
\usage{
eventModel_Gauss(M, s, refYear=NULL, studyPeriodMin, studyPeriodMax, numberChains = 2, 
        numberAdapt = 10000, numberUpdate = 10000, variable.names = c("theta"), 
        numberSample = 50000, thin = 10)
}
\arguments{
  \item{M}{vector of measurement}
  \item{s}{vector of measurement errors}
  \item{refYear}{vector of year of reference for ages}
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
\details{
If there are Nbobs measurements M associated with their error s, the model is the following one :
  \itemize{ 
    \item for j in (1:Nbobs) 
     \itemize{ 
     \item \code{Mj ~ N(muj, sj^2)}
     \item \code{muj ~ N(theta, sigmai^2)}
     \item \code{sigmai ~ UniformShrinkage }
     }
    \item \code{theta ~ U(ta, tb)}
  }
  
}
\references{
P. Lanos and A. Philippe. Hierarchical Bayesian Modeling for Combining Dates in Archaeological Context. Journal de la SFdS, Vol. 158 (2) pp 72-88 2017.
}
\examples{
  data(sunspot)
  MCMC = eventModel_Gauss(M=sunspot$Age[1:10], s= sunspot$Error[1:10], refYear=rep(2016,10), 
  studyPeriodMin=900, studyPeriodMax=1500, variable.names = c('theta'))
  plot(MCMC)
  gelman.diag(MCMC)
}
\keyword{ Event Model }
\keyword{ Gaussian dates }
\keyword{ Combination of dates}
\keyword{ Random effect }
