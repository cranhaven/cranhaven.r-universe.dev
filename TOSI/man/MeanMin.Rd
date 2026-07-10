\name{MeanMin}
\alias{MeanMin}
\title{Data splitting-based two-stage minimum mean testing method for the mean vector.}
\description{
  Conduct the simultaneous inference for a set of mean components in the the Null hypothesises H02 that assumes the set of mean components exist zero.
}
\usage{
  MeanMin(X, test.set, Nsplit = 5, frac.size=0.5, standardized=FALSE, alpha=0.05, seed=1)
}
\arguments{
  \item{X}{a \code{n}-by-\code{p} matrix, the observed data}
  \item{test.set}{a positive vector with values between 1 and p, the set of H02.}
  \item{Nsplit}{a positive integer, the random split times used, default as 5.}
  \item{frac.size}{a positive number between 0 and 1, the proportion of the sample used in stage I.}
  \item{standardized}{a logical value, whether standerdize in stage I.}
  \item{alpha}{a positive number, the significant level.}
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
  \code{\link{gendata_Mean}}, \code{\link{MeanMin}}
}
\examples{
  ### Example
  n <- 100; p <- 100; i <- 1
  s0 <- 5 # First five components are nonzeros
  rho <- 4; tau <- 1;
  dat1 <- gendata_Mean(n, p, s0, seed=i, rho, tau)
  # ex1: H01 is false
  MeanMin(dat1$X, 1:s0)
  MeanMin(dat1$X, 1:s0, Nsplit=1)
  # ex1: H01 is true
  MeanMin(dat1$X, 1:p)
  MeanMin(dat1$X, 1:p, Nsplit=1)
}
\keyword{Simultanous inference}
