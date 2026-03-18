\name{variance.par}
\alias{variance.par}
\title{Calculating the variance of the parameters}
\description{
Calculating the variance of the parameters of the estimation, depending on the second order derivative and the penalized second order derivative of the density estimation.
}
\usage{
variance.par(penden.env)
}
\arguments{
  \item{penden.env}{Containing all information, environment of pendensity()}
}
\details{
The variance of the parameters of the estimation results as the product of the inverse of the penalized second order derivative times the second order derivative without penalization time the inverse of the penalized second order derivative.

\eqn{V(\beta, \lambda_0)=I_p^{-1}(\beta, \lambda) I_p(\beta, \lambda=0) I_p^{-1}(\beta, \lambda)} with \eqn{I_p(\beta^{-1}, \lambda)=E_{f(y)}\bigl\{J_p(\beta, \lambda)\bigr\}}

The needed values are saved in the environment.
}
\value{
The return is a variance matrix of the dimension (K-1)x(K-1).
}
\author{Christian Schellhase <cschellhase@wiwi.uni-bielefeld.de>}
\references{Density Estimation with a Penalized Mixture Approach, Schellhase C. and Kauermann G. (2012), Computational Statistics 27 (4), p. 757-777.}
\keyword{nonparametric}

