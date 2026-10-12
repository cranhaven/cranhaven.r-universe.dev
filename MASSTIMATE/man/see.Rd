\name{see}
\alias{see}
\title{Standard Error of the Estimate}
\description{This function computes the standard error of the estimate based on the actual and predicted values}
\usage{see(true, pred)}
\arguments{
  \item{true}{a numeric vector representing the true values}
  \item{pred}{a numeric vector representing the predicted values, must be equal length to \code{true}}
}
\details{
This function calculates the standard error of the estimate (see) as a scaled residual, so:
  \deqn{
    see=\sqrt{\frac{\sum \left ( true-pred \right )^{2}}{N}}
  }{\sqrt\sum((true-pred)^2)/N}
}
\value{
Function returns a length one value of the standard error of the estimate
}
\author{Nicolas E. Campione}