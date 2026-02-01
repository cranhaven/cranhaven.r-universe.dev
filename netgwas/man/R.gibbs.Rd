\name{R.gibbs}
\alias{R.gibbs}

\title{ The expectation of covariance matrix using Gibbs sampling}

\description{
This function implements the Gibbs sampling method within Gaussian copula graphical model to estimate the conditional expectation for the data that not follow Gaussianity assumption (e.g. ordinal, discrete, continuous non-Gaussian, or mixed dataset).
}

\usage{
R.gibbs(y, theta, gibbs.iter = 1000, mc.iter = 500, 
                   ncores = NULL, verbose = TRUE)
}

\arguments{
	\item{y}{
	 An (\eqn{n \times p}) matrix or a \code{data.frame} corresponding to the data matrix (\eqn{n} is the sample size and \eqn{p} is the number of variables). 
	 It also could be an object of class \code{"simgeno"}.}
	\item{theta}{ A \eqn{p \times p} precision matrix. Default is a diagonal matrix.}
	\item{gibbs.iter}{ The number of burn-in for the Gibbs sampling. The default value is 1000.}
	\item{mc.iter}{ The number of Monte Carlo samples to calculate the conditional expectation. The default value is 500. }
	\item{ncores}{If \code{ncores = NULL}, the algorithm internally detects number of available cores and run the calculations in parallel on (available cores - 1). Typical usage is to fix \code{ncores = 1} when \eqn{p} is small \eqn{( p < 500 )}, and \code{ncores = NULL} when \eqn{p} is very large.}
	\item{verbose}{ If \code{verbose = FALSE}, printing information is disabled. The default value is \code{TRUE}.}
}

\details{
This function calculates \eqn{\bar{R}} using Gibbs sampling method within the E-step of EM algorithm, where 
\deqn{ \bar{R} = n ^ {-1} \sum_{i=1}^{n} E( Z^{(i)} Z^{(i)t} | y^{(i)}, \hat{\Theta}^{(m)})}
which \eqn{n} is the number of sample size and \eqn{Z} is the latent variable which is obtained from Gaussian copula graphical model.
}

\value{
\item{ES}{Expectation of covariance matrix ( diagonal scaled to 1) of the Gaussian copula graphical model}
}

\references{
1. Behrouzi, P., Arends, D., and Wit, E. C. (2023). netgwas: An R Package for Network-Based Genome-Wide Association Studies. The R journal, 14(4), 18-37.\cr
2. Behrouzi, P., and Wit, E. C. (2019). Detecting epistatic selection with partially observed genotype data by using copula graphical models. Journal of the Royal Statistical Society: Series C (Applied Statistics), 68(1), 141-160. \cr
}

\author{ 
  Pariya Behrouzi, Danny Arends and Ernst C. Wit \cr
  Maintainers: Pariya Behrouzi \email{pariya.behrouzi@gmail.com}\cr
}

\examples{
D <- simgeno(p = 100, n = 50, k = 3)
R.gibbs(D$data, ncores=1)
}
