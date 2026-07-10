\name{FacRowMinST}
\alias{FacRowMinST}
\title{Data splitting-based two-stage minimum testing method for a group of loading vectors in factor models.}
\description{
  Conduct the simultaneous inference for a set of loading vectors inr the NUll hypothesises H02 that assumes there is zero loading vector in the set of loading vectors.
}
\usage{
  FacRowMinST(X, G2,  q=NULL, Nsplit= 5, sub.frac=0.5,
              alpha=0.05, standardized=FALSE,seed=1)
}
\arguments{
  \item{X}{a \code{n}-by-\code{p} matrix, the observed data}
  \item{G2}{a positive vector with values between 1 and p, the set of H02.}
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
  G2 <- 1:200; # all are nonzero loading vectors
  FacRowMinST(X, G2=G2, alpha=0.05, sub.frac=0.5)
  FacRowMinST(X, q= 6, G2=G2, alpha=0.05, sub.frac=0.5) # specify the true number of factors
  # ex2: H01 is true
  G2 <- 1:500 # all are zero loading vectors
  FacRowMinST(X, G2=G2, alpha=0.05, sub.frac=0.5)
  FacRowMinST(X, q= 7, G2=G2, alpha=0.05, sub.frac=0.5) # specify a false number of factors
}
\keyword{Simultanous inference}
\keyword{Feature}
