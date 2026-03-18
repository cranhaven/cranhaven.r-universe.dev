\name{new.lambda}
\alias{new.lambda}
\title{Calculating new penalty parameter lambda}
\description{
Calculating new penalty parameter lambda. 
}
\usage{
new.lambda(penden.env,lambda0)
}

\arguments{
  \item{penden.env}{Containing all information, environment of pendensity()}
  \item{lambda0}{actual penalty parameter lambda}
}
\details{
Iterating for the lambda is stopped, when the changes between the old and the new lambda is smaller than 0.01*lambda0. If this criterion isn't reached, the iteration is terminated after 11 iterations.

The iteration formulae is
\deqn{\lambda^{-1}=\frac{\hat{\beta}^T D_m \hat{\beta}}{df(\hat{\lambda})-(m-1)}.}{%
 \eqn{\lambda=(\beta^T D_m \beta)/(df(\lambda)-(m-1)}}
}
\value{
Returning the new lambda.
}

\author{Christian Schellhase <cschellhase@wiwi.uni-bielefeld.de>}
\keyword{nonparametric}
\references{Density Estimation with a Penalized Mixture Approach, Schellhase C. and Kauermann G. (2012), Computational Statistics 27 (4), p. 757-777.}
