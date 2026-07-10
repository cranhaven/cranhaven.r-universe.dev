\name{FacRowMaxST}
\alias{FacRowMaxST}
\title{Data splitting-based two-stage maximum testing method for a group of loading vectors in factor models.}
\description{
  Conduct the simultaneous inference for a set of loading vectors in the NUll hypothesises H01 that assumes the set of loading vectors are all zeroes.
}
\usage{
  FacRowMaxST(X, G1, q=NULL, Nsplit= 5, sub.frac=0.5,
              alpha=0.05, standardized=FALSE,seed=1)
}
\arguments{
  \item{X}{a \code{n}-by-\code{p} matrix, the observed data}
  \item{G1}{a index set with values of components between 1 and p, the testing set in H01.}
  \item{q}{a positive integer, the number of factors. It will automatically selected by a criterion if it is NULL.}
  \item{Nsplit}{a positive integer, the number of data spliting, default as 5.}
  \item{sub.frac}{a positive number between 0 and 1, the proportion of the sample used in stage I.}
  \item{alpha}{a positive real, the significance level.}
  \item{standardized}{a logical value, whether use the standardized test statistic.}
  \item{seed}{a non-negative integer, the random seed.}
}


\value{
  return a vector with names 'CriticalValue', 'TestStatistic', 'reject_status', 'p-value' if Nsplit=1, and 'reject_status' and 'adjusted_p-value' if Nsplit>1.
}
\references{Wei Liu, Huazhen Lin, Jin Liu (2020). Estimation and inference on high-dimensional sparse factor models.
}
\author{
  Liu Wei
}
\note{
  nothing
}
\seealso{
  \code{\link{Factorm}}
}
\examples{
  ### Example
  dat <- gendata_Fac(n = 300, p = 500)
  res <- Factorm(dat$X)
  X <- dat$X
  # ex1: H01 is false
  G1 <- 1:10; # all are nonzero loading vectors
  FacRowMaxST(X, G1=G1, alpha=0.05, sub.frac=0.5)
  FacRowMaxST(X, q= 6, G1=G1, alpha=0.05, sub.frac=0.5) # specify the true number of factors
  # ex2: H01 is true
  G1 <- 481:500 # all are zero loading vectors
  FacRowMaxST(X, G1=G1, alpha=0.05, sub.frac=0.5)
  FacRowMaxST(X, q= 7, G1=G1, alpha=0.05, sub.frac=0.5) # specify a false number of factors
}
\keyword{Simultanous inference}
\keyword{Feature}
