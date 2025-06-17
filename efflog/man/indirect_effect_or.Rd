\name{indirect_effect_or}
\alias{indirect_effect_or}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Indirect effect for loglinear models both with the multiplicative interaction and without it
}
\description{
This function calculates the indirect effect using the odds ratio for any loglinear model and under dummy code parametrization
}
\usage{
indirect_effect_or(x, y, z, w, t)
}

%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{it is the parameter \eqn{\mu^{Y=1}}}
  \item{y}{it is the parameter \eqn{\mu^{X=1,Y=1}}}
  \item{z}{it is the parameter \eqn{\mu^{Z=1, Y=1}}}
  \item{w}{it is the parameter \eqn{\mu_c^{Z=1}}}
  \item{t}{it is the parameter \eqn{\mu_c^{X=1,Z=1}}}

}
\value{ indirect_effect_or returns the indirect effect}
\references{Gheno Gloria (2015),The causal analysis in the loglinear model 
}
\author{Gloria Gheno
}

\examples{
indirect_effect_or(0.6,0.8,0.5,2,0.7)


}



