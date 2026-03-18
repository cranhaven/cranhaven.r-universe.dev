\name{HPDcrd}
\alias{HPDcrd}
\title{
generates the 100 * (1 - alpha)\% most probable interval of a distribution of empirical values
}
\description{
  function used to produce a Bayesian credible interval of a unimodal
  distribution of empirical values using the Highest Posterior
  Probability approach
}
\usage{
HPDcrd(x, alpha = 0.05)
}

\arguments{
  \item{x}{
a vector of empirical values
}
\item{alpha}{
  1 - alpha is the desired level of credibility
}
}
\value{
  a vector of the lower and upper limits of   the 95\% credible interval
  calculated using a standard algorithm
}
\references{
  van Hulst, R. 2018. Evaluating Scientific Evidence. ms.
}
\author{
Robert van Hulst
}
\examples{
HPDcrd(rnorm(1000))
}

\keyword{ distribution }

