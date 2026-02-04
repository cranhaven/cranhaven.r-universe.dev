\name{weibullsim_pw}
\alias{weibullsim_pw}
\title{Simulate event time with weibull hazard function. The parameters can be piecewise.}
\usage{
weibullsim_pw(lambda,gamma,t,x,betas=NULL)


}

\arguments{

  \item{lambda}{lambda for event hazard function. It will be a vector for piecewise hazard function.}
  \item{gamma}{gamma for event hazard function. It will be a vector for piecewise hazard function.}
  \item{x}{design matrix}
  \item{betas}{covariates for desing matrix}
  \item{t}{time intervals for piecewise weibull parameters.}
}


\description{
Simulate event time with weibull hazard function given the piecewise parameters.
}

