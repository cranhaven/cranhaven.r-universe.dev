\name{simdata}
\alias{simdata}
\docType{data}
\title{Simulated Data from a Scaled Von Mises Distribution with Noise
}
\description{
This simulated data set contains n=5000 observations from a scaled Von Mises distribution with noise (\eqn{\kappa = 1}; c = 0.3; \eqn{noise = 0.2}). 
Similar data sets can be generated with the function \code{\link{von_mises_sim}}.
}
\usage{data(simdata)}
\format{A vector containing 5000 observations.}
\source{
Schutte WD (2014). Nonparametric estimation of the off-pulse interval(s) of a pulsar light
curve. Ph.D. thesis, North-West University. URL \url{http://hdl.handle.net/10394/12199} \cr
}
\examples{
data(simdata)
hist(simdata)
SOPIE(simdata)
}
\keyword{datasets}
