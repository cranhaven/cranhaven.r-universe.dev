\name{LambdaCensor}
\alias{LambdaCensor}
\title{Find the Censor Hazard Function Parameter for Proportional Hazard Model Given a Fixed Dropout Rate}
\usage{
LambdaCensor(lambda=lambda,gamma=gamma,theta=theta,ebx=1,gammac=1,
  groupfreq=1,censordist='exponential',timeinterval=NULL)
}

\arguments{

  \item{lambda}{lambda for event hazard function}
  \item{gamma}{gamma for event hazard function}
  \item{gammac}{gamma for censor hazard function. This is required if the censoring hazard function is weibull}
  \item{theta}{Dropout rate}
  \item{ebx}{exp(beta*x), if there's no covariates, ebx=1.}
  \item{groupfreq}{frequence of each level of ebx value}
  \item{censordist}{censor hazard function distribution}
  \item{timeinterval}{time intervals for piecewise baseline hazard function}

}
\examples{

lambdac<-LambdaCensor(lambda=0.03,gamma=1,theta=0.2)

}

\description{
calculate censor hazard function parameter for a given dropout rate.
}

\references{
Wan F. (2017) \emph{Simulating survival data with predefined censoring rates for proportional hazards models. Statist. Med. 2017; 36(5): 838-854}

Martinez EZ, Achcar JA, de Oliveira Peres MV, de Queiroz JAM (2016) \emph{ A brief note on the simulation of survival data with a desired percentage of right-censored data. Journal of Data Science . 2016, Vol. 14 Issue 4, p701-712. 12p}
}

