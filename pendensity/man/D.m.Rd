\name{D.m}
\alias{D.m}

\title{Calculating the penalty matrix}
\description{

calculating the penalty matrix depending on the number of covariates 'p', the order of differences to be penalized 'm', the corresponding difference matrix 'L' of order 'm', the covariate matrix 'Z', the number of observations 'n' and the number of knots 'K'.
}
\usage{
D.m(penden.env)
}

\arguments{
\item{penden.env}{Containing all information, environment of pendensity()}
}

\details{The penalty matrix is calculated as

\eqn{D_m=(L^T \otimes I_p) (I_{K-m} \otimes \frac{Z^T Z}{n}) (L \otimes I_p)}

The needed values are saved in the environment.
}

\value{
Returning the penalty matrix.
}
\references{Density Estimation with a Penalized Mixture Approach, Schellhase C. and Kauermann G. (2012), Computational Statistics 27 (4), p. 757-777.}

\author{Christian Schellhase <cschellhase@wiwi.uni-bielefeld.de>}

\keyword{nonparametric}

