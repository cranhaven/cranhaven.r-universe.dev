\name{combinationWithOutliers_Gauss}
\alias{combinationWithOutliers_Gauss}
\title{
Bayesian modeling for combining Gaussian dates and handling outliers
}
\description{
Bayesian modeling for combining Gaussian dates with known variance and that may be outliers. These dates are assumed to be contemporaneous of the target date and have non identical distributions as the variance may be different for each date. The posterior distribution of the modeling is sampled by a MCMC algorithm implemented in JAGS. 
}
\usage{
combinationWithOutliers_Gauss(M, s, refYear=NULL, outliersIndivVariance, 
        outliersBernouilliProba, studyPeriodMin, studyPeriodMax, numberChains = 2, 
        numberAdapt = 10000, numberUpdate = 10000, variable.names = c("theta"), 
        numberSample = 50000, thin = 10)
}
\arguments{
  \item{M}{vector of measurement}
  \item{s}{vector of measurement errors}
  \item{refYear}{vector of year of reference for ages}
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
This function returns a Markov chain of the posterior distribution. The MCMC chain is in date format BC/AD, that is the reference year is 0. Only values for the variables defined by 'variable.names' are given.
}
\details{
If there are Nbobs measurements M associated with their error s, the model is the following one :
  \itemize{ 
    \item \code{for j in (1:Nbobs)}
      \itemize{ 
      \item \code{Mj ~ N(muj, sj^2)}
      \item \code{muj <- theta + deltaj * phij}

      \item \code{deltaj ~ N(0, sigma.deltaj^2)}
      \item \code{phij ~ Bern(pj)}
      }
  \item \code{theta ~ U(ta, tb)}
  }
  
}
\author{
Anne Philippe & Marie-Anne Vibet
}
\references{
Bronk Ramsey C., Dealing with outliers and offsets in Radiocarbon dating, Radiocarbon, 2009, 51:1023-45.
}
\examples{
  data(sunspot)
  MCMC1 = combinationWithOutliers_Gauss(M=sunspot$Age[1:10], s= sunspot$Error[1:10], 
  refYear=rep(2016,10), outliersIndivVariance = rep(1,10), 
  outliersBernouilliProba=rep(0.2, 10), studyPeriodMin=800, studyPeriodMax=1500, 
  variable.names = c('theta'))
  plot(MCMC1)
  gelman.diag(MCMC1)
  
  # Influence of outliersIndivVariance
  MCMC2 = combinationWithOutliers_Gauss(M=sunspot$Age[1:10], s= sunspot$Error[1:10], 
  refYear=rep(2016,10), outliersIndivVariance = rep(10,10), 
  outliersBernouilliProba=rep(0.2, 10), studyPeriodMin=800, studyPeriodMax=1500, 
  variable.names = c('theta'))
  plot(MCMC2)
  gelman.diag(MCMC2)  

}
\keyword{ Combination of dates}
\keyword{ Gaussian dates }
\keyword{ Outliers }