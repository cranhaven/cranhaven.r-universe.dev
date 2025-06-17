\name{cell_effect_or}
\alias{cell_effect_or}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Cell effect for loglinear models without the multiplicative interaction
}
\description{
This function calculates the cell effect using the odds ratio for a loglinear model
without the multiplicative interaction and under dummy code parametrization
}
\usage{
cell_effect_or(x, y, z, w)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{it is the parameter \eqn{\mu^{Y=1}}}
  \item{y}{it is the parameter \eqn{\mu^{X=1,Y=1}}}
  \item{z}{it is the parameter \eqn{\mu^{Z=1, Y=1}}}
  \item{w}{it is the parameter \eqn{\mu_c^{Z=1}}}
}
\value{ cell_effect_or returns the cell effect}
\references{Gheno Gloria (2015),The causal analysis in the loglinear model 
}
\author{Gloria Gheno
}
\examples{
cell_effect_or(0.6,0.8,0.5,2)

}




