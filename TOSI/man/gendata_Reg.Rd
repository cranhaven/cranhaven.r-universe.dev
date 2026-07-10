\name{gendata_Reg}
\alias{gendata_Reg}
\title{Generate simulated data}
\description{
  Generate simulated data from  high-dimensional sparse regression model.
}
\usage{
  gendata_Reg(n=100, p = 20, s0=5, rho=1, seed=1)

}
\arguments{
  \item{n}{a positive integer, the sample size, default as 100.}
  \item{p}{an positive integer, the dimension of covriates, default as 20.}
  \item{s0}{a positive integer, the number of nonzero components of regression coefficients, default as 5.}
  \item{rho}{a positive number, controlling the magnitude of coefficients.}
  \item{seed}{a nonnegative integer, the random seed, default as 1.}
}

\value{
  return a list including two components:

  \item{Y}{a \code{n}-dimensional vector, the observed response vector.}
  \item{X}{a \code{n}-by-\code{p} matrix, the observed covariates matrix.}
  \item{beta0}{a p-dimensional vector, the Reg. coefficients.}
  \item{index_nz}{a integer vector, the index of nonzero components of Reg. coefficients.}
}

\author{
  Liu Wei
}
\note{
  nothing
}
\examples{
  dat <- gendata_Reg(n=100, p = 100, s0=3)
  str(dat)
}
