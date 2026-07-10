\name{RegMin}
\alias{RegMin}
\title{Data splitting-based two-Stage minimum testing method for the regression coefficients in linear regression models.}
\description{
  Conduct the simultaneous inference for a set of regression coefficients in a null hypothesises H02 that assumes the set of regression coefficients components exist zero.
}
\usage{
  RegMin(X, Y,  G2, Nsplit = 5, sub.frac=0.5, alpha=0.05, seed=1,  standardized=FALSE)
}
\arguments{
  \item{X}{a \code{n}-by-\code{p} matrix, the observed covariates matrix.}
  \item{Y}{a \code{n}-dimensional vector, the observed outcome vector.}
  \item{G2}{a positive vector with values between 1 and p, the set of regression coefficients in the null hypothesises H02.}
  \item{Nsplit}{a positive integer, the random split times used, default as 5.}
  \item{sub.frac}{a positive number between 0 and 1, the proportion of the sample used in the stage I.}
  \item{alpha}{a positive real, the significance level.}
  \item{seed}{a non-negative integer, the random seed.}
   \item{standardized}{a logical value, whether standerdize the covariates matrix in the stage I.}
}

\value{
  return a vector with names 'CriticalValue', 'TestStatistic', 'reject_status', 'p-value' if Nsplit=1, and 'reject_status' and 'adjusted_p-value' if Nsplit>1.
}
\references{Liu, W., Lin, H., Liu, J., & Zheng, S. (2020). Two-directional simultaneous inference for high-dimensional models. arXiv preprint arXiv:2012.11100.
}
\author{
  Liu Wei
}
\note{
  nothing
}
\seealso{
  \code{\link{gendata_Reg}}
}
\examples{
  \donttest{
  ### Example
  n <- 100; p <- 20;i <- 1
  s0 <- 5 # First five components are nonzeros
  rho <- 1;
  dat1 <- gendata_Reg(n, p, s0, seed=i, rho)
  # ex1: H01 is false
  RegMin(dat1$X, dat1$Y, 1:s0)
  # ex1: H01 is true
  RegMin(dat1$X, dat1$Y, p)
  }
}
\keyword{Simultanous inference}
