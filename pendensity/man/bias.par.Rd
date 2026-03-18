\name{bias.par}
\alias{bias.par}
\title{Calculating the bias of the parameter beta}
\description{
Calculating the bias of the parameter beta.
}
\usage{
bias.par(penden.env)
}

\arguments{
  \item{penden.env}{Containing all information, environment of pendensity()}
}
\details{
The bias of the parameter beta is calculated as the product of the penalty parameter lambda, the penalized second order derivative of the log likelihood function w.r.t. beta 'Derv.pen', the penalty matrix 'Dm' and the parameter set 'beta'.
\deqn{Bias(\beta)= - \lambda  {Derv2.pen(\beta)}^{-1} D_m \beta}{%
\eqn{Bias(beta)=lambda Derv2.pen(beta)^-1 Dm beta}}

The needed values are saved in the environment.
}

\value{
Returning the bias of the parameter beta.
}
\references{Density Estimation with a Penalized Mixture Approach, Schellhase C. and Kauermann G. (2012), Computational Statistics 27 (4), p. 757-777.}
\author{Christian Schellhase <cschellhase@wiwi.uni-bielefeld.de>}

\keyword{nonparametric}
