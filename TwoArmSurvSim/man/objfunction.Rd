\name{objfunction}
\alias{objfunction}
\title{Objective Function for the Finding of Censor Hazard Function Parameter for Proportional Hazard Model}
\usage{
objfunction(x,lambda,gamma,gammac,theta,ebx,groupfreq,censordist,timeinterval)
}

\arguments{
  \item{x}{Censor hazard function parameter. For exponential or weibull censor hazard, x is lambda, for uniform hazard,  x is the maximum time of the censoring window.}

  \item{lambda}{lamda for event hazard function}
  \item{gamma}{gamma for event hazard function}
  \item{gammac}{gamma for censor hazard function. THis is required if the censoring hazard function is weibull}
  \item{theta}{Dropout rate}
    \item{ebx}{exp(beta*x), if there's no covariates, ebx=1.}
  \item{groupfreq}{frequence of each level of ebx value}
  \item{censordist}{censor hazard function distribution}
  \item{timeinterval}{time intervals for piecewise baseline hazard function}
}



\description{
objective function for the finding of censor hazard function parameter.
}
