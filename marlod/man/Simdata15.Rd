\name{simdata15}
\alias{simdata15}
\docType{data}
\title{Simulated Dataset 15}
\description{The 15th dataset from the simulation study has 100 subjects (sample size is 30). Each subject has three repeated measurements. The independent variables or covariates are simulated from a Bernoulli distribution with a parameter value of \eqn{p} = 0.5 and a uniform distribution \eqn{U}(0, 1), respectively. Correlated errors for models with repeated measures are accounted for and assumed to follow a multivariate normal distribution, \eqn{MVN}(0, \eqn{R(\alpha})). A first-order autoregressive (AR-1) correlation structure with a correlation parameter of \eqn{\alpha} = 0.7 is incorporated into the multivariate normal distribution. The true values of 1, 1, and 1 are corresponded to the marginal intercept and two slopes, accordingly.
}
\usage{data("simdata15")}
\format{
  A data frame with 30 subjects and each subject has three repeated measurements, i.e., number of cluster size or time points. A list that contains two variables:
  \describe{
    \item{\code{y}}{A column matrix of the continuous outcome values.}
    \item{\code{int}}{A column matrix of the intercept values of one.}
    \item{\code{x1}}{A column matrix of the binary covariate values that follow a Bernoulli distribution.}
    \item{\code{x2}}{A column matrix of the continuous covariate values that follow a uniform distribution.}
    \item{\code{id}}{A column matrix of the numbers of identification.}
    \item{\code{visit}}{A column matrix of the order of cluster size or time points.}
  }
}
\examples{
library(marlod)
data(simdata15)
}
\keyword{datasets}
