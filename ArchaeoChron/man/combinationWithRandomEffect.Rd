\name{combinationWithRandomEffect_Gauss}
\alias{combinationWithRandomEffect_Gauss}
\title{
Bayesian modeling for combining Gaussian dates with a random effect
}
\description{
Bayesian modeling for combining Gaussian dates with known variance and with the addition of a random effect. These dates are assumed to be contemporaneous of the target date and have non identical distributions as the variance may be different for each date. In addition, a random effect is introduced in the modelling by a shrinkage distribution as defined by Congdom (2010). 
The posterior distribution of the modeling is sampled by a MCMC algorithm implemented in JAGS. 
}
\usage{
combinationWithRandomEffect_Gauss(M, s, refYear=NULL, studyPeriodMin, studyPeriodMax, 
        numberChains = 2, numberAdapt = 10000, numberUpdate = 10000, 
        variable.names = c("theta"), numberSample = 50000, thin = 10)
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
\details{
If there are Nbobs measurements M associated with their error s,  the model is the following one :

  \itemize{ 
    \item for j in (1:Nbobs) 
     \itemize{ 
     \item \code{Mj ~ N(muj, sj^2)}
     \item \code{muj ~ N(theta, sigmai^2)}
     }
    \item \code{theta ~ U(ta, tb)}
    \item \code{sigma ~ UniformShrinkage }
  }
  
}

\author{
Anne Philippe & Marie-Anne Vibet
}
\references{
Congdom P. D., Bayesian Random Effect and Other Hierarchical Models: An Applied Perspective,Chapman and Hall/CRC, 2010
}
\examples{
  data(sunspot)
  MCMC = combinationWithRandomEffect_Gauss(M=sunspot$Age[1:10], s= sunspot$Error[1:10], 
  refYear=rep(2016,10), studyPeriodMin=0, studyPeriodMax=1500, variable.names = c('theta'))
  plot(MCMC)
  gelman.diag(MCMC)
}
\keyword{ Combination of dates}
\keyword{ Gaussian dates }
\keyword{ Random effect }

