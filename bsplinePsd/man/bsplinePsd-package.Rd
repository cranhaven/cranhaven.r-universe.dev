\name{bsplinePsd-package}
\alias{bsplinePsd-package}
\alias{bsplinePsd}
\docType{package}
\title{
Bayesian Nonparametric Spectral Density Estimation Using B-Spline Priors
}
\description{
Implementation of a Metropolis-within-Gibbs MCMC algorithm to flexibly estimate the spectral density of a stationary time series.  The algorithm updates a nonparametric B-spline prior using the Whittle likelihood to produce pseudo-posterior samples.
}
\details{
The function \code{gibbs_bspline} is an implementation of the (serial version of the) MCMC algorithm presented in Edwards et al. (2018).  This algorithm uses a nonparametric B-spline prior to estimate the spectral density of a stationary time series and can be considered a generalisation of the algorithm of Choudhuri et al. (2004), which used the Bernstein polynomial prior.  A Dirichlet process prior is used to find the weights for the B-spline densities used in the finite mixture and a seperate and independent Dirichlet process prior used to place knots.  The algorithm therefore allows for a data-driven choice of the number of knots/mixture components and their locations.
}
\author{
Matthew C. Edwards, Renate Meyer, Nelson Christensen

Maintainer:  Matthew C. Edwards <matt.edwards@auckland.ac.nz>
}
\references{
Edwards, M. C., Meyer, R., and Christensen, N. (2018), Bayesian nonparametric spectral density estimation using B-spline priors, \emph{Statistics and Computing}, <https://doi.org/10.1007/s11222-017-9796-9>.

Choudhuri, N., Ghosal, S., and Roy, A. (2004), Bayesian estimation of the spectral density of a time series, \emph{Journal of the American Statistical Association}, 99(468):1050--1059.
}
\keyword{ package }
