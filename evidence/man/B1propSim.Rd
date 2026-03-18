\name{B1propSim}
\alias{B1propSim}
\title{
simulates Bayesian updating of the binomial parameter \eqn{\pi}.
}
\description{
Provides a simple demonstration of how the posterior distribution
improves as increasing amounts of data become available. A Binomial
variable with a known parametric probability is sampled, and as
increasing numbers of samples become available the posterior
distribution is re-evaluated and plotted.
}
\usage{
B1propSim(p, N = 100, prior = c("uniform", "near_0.5",
  "not_near_0.5", "near_0", "near_1"))
}
\arguments{
  \item{p}{
the ``real'' binomial probability; if a number samller than 0 or one
lager than 1 isentered the function will choose an arbitrary probability
}
  \item{N}{
the number of observations to accumulate
}
  \item{prior}{
one of: "uniform", "near_0.5", "not_near_0.5", "near_0", or "near_1".
}
}
\value{
none returned; the function is run for the plot it produces.
}
\references{
van Hulst, R. 2018. Evaluating Scientific Evidence. ms.
}
\author{
Robert van Hulst
}
\examples{
B1propSim(p = 0.44, prior = "near_0.5")
}
