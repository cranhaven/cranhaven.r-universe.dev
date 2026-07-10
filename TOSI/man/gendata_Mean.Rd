\name{gendata_Mean}
\alias{gendata_Mean}
\title{Generate simulated data}
\description{
  Generate simulated data from  for high-dimensional mean model.
}
\usage{
  gendata_Mean(n, p, s0= floor(p/2), seed=1, rho= 1, tau=1)

}
\arguments{
  \item{n}{a positive integer, the sample size.}
  \item{p}{an positive integer, the variable dimension.}
  \item{s0}{a positive integer, the number of nonzero components of mean .}
  \item{seed}{a nonnegative integer, the random seed, default as 1.}
  \item{rho}{a positive number between 0 and 1, controlling the correlation of data.}
  \item{tau}{a positive number, controlling the magnitude of covriance matrix.}
}

\value{
  return a list including two components:

    \item{X}{a \code{n}-by-\code{p} matrix, the observed data matrix.}
  \item{mu}{a p-dimensional vector, the mean vector.}
  \item{p0}{a integer vector, the number of nonzero components of mean.}
}

\author{
  Liu Wei
}
\note{
  nothing
}
\examples{
  dat <- gendata_Mean(n=100, p = 100, s0=3)
  str(dat)
}
