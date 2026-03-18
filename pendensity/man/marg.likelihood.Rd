\name{marg.likelihood}
\alias{marg.likelihood}

\title{Calculating the marginal likelihood}

\description{
Calculating the marginal likelihood.
}

\usage{
marg.likelihood(penden.env,pen.likelihood)
}

\arguments{
  \item{penden.env}{Containing all information, environment of pendensity()}
  \item{pen.likelihood}{penalized log likelihood}
}
\details{
Calculating is done using a Laplace approximation for the integral of the marginal likelihood.

The needed values are saved in the environment.
}
\value{
Returning the marginal likelihood.
}
\author{Christian Schellhase <cschellhase@wiwi.uni-bielefeld.de>}
\references{Density Estimation with a Penalized Mixture Approach, Schellhase C. and Kauermann G. (2012), Computational Statistics 27 (4), p. 757-777.}
\keyword{math}

