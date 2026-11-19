\name{BEAST}
\alias{BEAST}
\title{Binary Expansion Adaptive Symmetry Test}
\usage{
BEAST(
  X,
  dep,
  subsample.percent = 1/2,
  B = 100,
  unif.margin = FALSE,
  lambda = NULL,
  index = list(c(1:ncol(X))),
  method = "p",
  num = NULL
)
}
\arguments{
\item{X}{a matrix to be tested.}

\item{dep}{depth of the binary expansion for the \code{BEAST}.}

\item{subsample.percent}{Fraction of observations in each subsample. The effective size is the positive integer truncation of n * subsample.percent.}

\item{B}{Number of subsamples.}

\item{unif.margin}{logicals. If \code{TRUE} the marginal distribution is known to be Uniform[0,1]. Default is \code{FALSE}, and empirical cdf transformation will be applied to each marginal distribution.}

\item{lambda}{tuning parameter for soft-thresholding, default to be \eqn{\sqrt{\log(2^{p \cdot dep}) / (8n)}.}}

\item{index}{
  a list of indices. If provided, test the independence among two or more groups of variables. For example, \code{index = list(c(1,2), c(3)))} refers to test the independence between \eqn{(X_1, X_2)} and \eqn{X_3}. Default to be \code{list(c(1:p))} to test if the data follow the multivariate uniform distribution over \eqn{[0,1]^p}, where \code{p = ncol(X)}.
}
\item{method}{
  If \code{"p"}, then compute null distribution with permutations. If \code{"s"}, then compute null distribution with simulations. If \code{"stat"}, only return interaction and BEAST Statistic. The \code{method = "s"} option is only available for testing uniformity and bivariate independence.
}
\item{num}{
  number of permutations if method == "p" (default to be 100), or simulations if method == "s" (supply an explicit positive value).
}
}

\value{
\item{Interaction}{the most frequent interaction among all subsamples.}
\item{BEAST.Statistic}{BEAST statistic.}
\item{Null.Distribution}{simulated null distribution.}
\item{p.value}{simulated p-value.}
}

\description{
\code{BEAST} (Binary Expansion Adaptive Symmetry Test) is used for nonparametric detection of nonuniformity or dependence.
}

\examples{
## Elapsed times 7.32 secs
## Measured in R 4.0.2, 32 bit, on a processor 3.3 GHz 6-Core Intel Core i5 under MacOS, 2024/9/6
\dontrun{
  x1 = runif(128)
  x2 = runif(128)
  y = sin(4*pi*(x1 + x2)) + 0.8*rnorm(128)
  ##test independence between (x1, x2) and y
  BEAST(cbind(x1, x2, y), 3, index = list(c(1,2), c(3)))
  ##test mutual independence among x1, x2 and y
  BEAST(cbind(x1, x2, y), 3, index = list(1, 2, 3))
  
  ##test bivariate uniformity
  x1 = rbeta(128, 2, 4)
  x2 = rbeta(128, 2, 4)
  BEAST(cbind(x1, x2), 3)
  ##test multivariate uniformity
  x1 = rbeta(128, 2, 4)
  x2 = rbeta(128, 2, 4)
  x3 = rbeta(128, 2, 4)
  BEAST(cbind(x1, x2, x3), 3)
}
}
