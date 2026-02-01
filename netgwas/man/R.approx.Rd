\name{R.approx}
\alias{R.approx}

\title{ The expectation of covariance using approximation method }

\description{
This function implements the approximation method within the Gaussian copula graphical model to estimate the conditional expectation for the data that not follow Gaussianity assumption (e.g. ordinal, discrete, continuous non-Gaussian, or mixed dataset).
}

\usage{
R.approx(y, Z = NULL, Sigma=NULL, rho = NULL, ncores = NULL )
}

\arguments{
	\item{y}{An (\eqn{n \times p}) matrix or a \code{data.frame} corresponding to the data matrix (\eqn{n} is the sample size and \eqn{p} is the number of variables). 
	 It also could be an object of class \code{"simgeno"}.}
	\item{Z}{ A (\eqn{n \times p}) matrix which is a transformation of the data via the Gaussian copula. If \code{Z = NULL} internally calculates an initial value for \eqn{Z}.}
	\item{Sigma}{The covariance matrix of the latent variable given the data. If \code{Sigma = NULL} the Sigma matrix is calculated internally with a desired penalty term, \code{rho}.}
	\item{rho}{A (non-negative) regularization parameter to calculate \code{Sigma}. rho=0 means no regularization.}
	\item{ncores}{If \code{ncores = NULL}, the algorithm internally detects number of available cores and run the calculations in parallel on (available cores - 1). Typical usage is to fix \code{ncores = 1} when \eqn{p} is small \eqn{( p < 500 )}, and \code{ncores = NULL} when \eqn{p} is large.}
}

\value{
\item{ES}{Expectation of covariance matrix( diagonal scaled to 1) of the Gaussian copula graphical model.}
\item{Z}{ New transformation of the data based on given or default \code{Sigma}.}
}

\author{ 
  Pariya Behrouzi and Ernst C. Wit \cr
  Maintainer: Pariya Behrouzi \email{pariya.behrouzi@gmail.com}
}

\references{
1. Behrouzi, P., Arends, D., and Wit, E. C. (2023). netgwas: An R Package for Network-Based Genome-Wide Association Studies. The R journal, 14(4), 18-37.\cr
2. Behrouzi, P., and Wit, E. C. (2019). Detecting epistatic selection with partially observed genotype data by using copula graphical models. Journal of the Royal Statistical Society: Series C (Applied Statistics), 68(1), 141-160. \cr
}

\examples{
\dontrun{
D <- simgeno(p = 90, n = 50, k = 3)
R.approx(D$data)
} 
}

