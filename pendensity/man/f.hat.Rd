\name{f.hat}
\alias{f.hat}
\title{Calculating the actual fitted values 'f.hat' of the estimated density function f for the response y}
\description{
Calculating the actual fitted values of the response, depending on the actual parameter set beta
}
\usage{
f.hat(penden.env,ck.temp=NULL)
}
\arguments{
  	\item{penden.env}{Containing all information, environment of pendensity()}
	\item{ck.temp}{actual weights, depending on the actual parameter set beta. If NULL, the beta parameter is caught in th environment}
}
\details{
Calculating the actual fitted values of the response, depending on the actual parameter set beta. Multiplying the actual set of parameters \eqn{c_k} with the base 'base.den' delivers the fitted values, depending on the group of covariates, listed in 'x.factor'.
}
\value{
The returned value is a vector of the fitted value for each observation of y.
}

\author{Christian Schellhase <cschellhase@wiwi.uni-bielefeld.de>}
\keyword{nonparametric}
\references{Density Estimation with a Penalized Mixture Approach, Schellhase C. and Kauermann G. (2012), Computational Statistics 27 (4), p. 757-777.}
