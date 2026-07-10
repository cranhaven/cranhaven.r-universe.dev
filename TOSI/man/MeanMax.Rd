\name{MeanMax}
\alias{MeanMax}
\title{Data splitting-based two-stage maximum mean testing method for the mean vector.}
\description{
  Conduct the simultaneous inference for a set of mean components in the NUll hypothesises H01 that assumes the set of mean components are all zeroes.
}
\usage{
  MeanMax(X, test.set, Nsplit = 5,frac.size=0.5, standardized=FALSE,alpha=0.05, seed=1)
}
\arguments{
  \item{X}{a \code{n}-by-\code{p} matrix, the observed data}
  \item{test.set}{a positive vector with values between 1 and p, the set of H01.}
  \item{Nsplit}{a positive integer, the random split times used, default as 5.}
  \item{frac.size}{a positive real between 0 and 1, the proportion of the sample used in stage I.}
  \item{standardized}{a logical value, whether standerdize variables in stage I.}
  \item{alpha}{a positive real, the significant level.}
  \item{seed}{a non-negative integer, the random seed.}
}

\value{
  return a vector with names 'CriticalValue', 'TestStatistic', 'reject_status', 'p-value' if Nsplit=1, and 'reject_status' and 'adjusted_p-value' if Nsplit>1.
}


\author{
  Liu Wei
}
\note{
  nothing
}
\seealso{
  \code{\link{gendata_Mean}}
}
\examples{
  ### Example
  n <- 100; p <- 100;i <- 1
  s0 <- 5 # First five components are nonzeros
  rho <- 1; tau <- 1;
  dat1 <- gendata_Mean(n, p, s0, seed=i, rho, tau)
  # ex1: H01 is false
  MeanMax(dat1$X, 1:p)
  MeanMax(dat1$X, 1:p, Nsplit=1)
  # ex1: H01 is true
  MeanMax(dat1$X, p)
  MeanMax(dat1$X, p, Nsplit=1)
}
\keyword{Simultanous inference}
