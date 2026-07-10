\name{RegMax}
\alias{RegMax}
\title{Data splitting-based two-stage maximum testing method for the regression coefficients in linear regression models}
\description{
  Conduct the simultaneous inference for a set of regression coefficients in the null hypothesises H01 that assume the set of regression coefficients components are all zeroes.
}
\usage{
  RegMax(X, Y,  G1, Nsplit = 5, sub.frac=0.5, alpha=0.05, seed=1,  standardized=FALSE)
}
\arguments{
  \item{X}{a \code{n}-by-\code{p} matrix, the observed covariates matrix.}
  \item{Y}{a \code{n}-dimensional vector, the observed response vector.}
  \item{G1}{a positive vector with values between 1 and p, the set of H01.}
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
    n <- 50; p <- 20; i <- 1
    s0 <- 5 # First five components are nonzeros
    rho <- 1;
    dat1 <- gendata_Reg(n, p, s0, seed=i, rho)
    # ex1: H01 is false
    RegMax(dat1$X, dat1$Y, 1:p)
    # ex1: H01 is true
    RegMax(dat1$X, dat1$Y, p)
  }
}
\keyword{Simultanous inference}
