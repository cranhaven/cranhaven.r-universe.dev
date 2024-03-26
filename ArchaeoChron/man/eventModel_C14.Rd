\name{eventModel_C14}
\alias{eventModel_C14}
\title{
Bayesian modeling for combining radiocarbon dates using the Event Model
}
\description{
Bayesian modeling for combining radiocarbon dates. These dates are assumed to be contemporaneous of the event date. The posterior distribution of the event date is sampled by MCMC algorithm as well as those of all parameters of the Bayesian model as described in Lanos & Philippe (2017). 
}
\usage{
eventModel_C14(M, s, calibCurve='intcal13', studyPeriodMin, studyPeriodMax, 
          numberChains = 2, numberAdapt = 10000, numberUpdate = 10000, 
          variable.names = c("theta"), numberSample = 50000, thin = 10)
}
\arguments{
  \item{M}{vector of radiocarbon measurements in date format Before Present (Ages before 1950)}
  \item{s}{vector of measurement errors}
  \item{calibCurve}{the name of the calibration curve associated with the M radiocarbon measurements}
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
  data(cuers)
  MCMC = eventModel_C14(M=cuers$Age, s=cuers$Error, calibCurve = 'intcal13', 
  studyPeriodMin = 1000, studyPeriodMax = 2000, variable.names = c('theta'), numberAdapt = 1000, 
  numberUpdate = 1000, numberSample = 3000)
  plot(MCMC)
}
\keyword{ Combination of dates}
\keyword{ Random effect }
\keyword{ Event Model }
\keyword{ Radiocarbon dates }

