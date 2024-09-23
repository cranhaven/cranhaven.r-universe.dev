\name{inferf}

\alias{inferf}

\title{
Posterior distribution of the \ifelse{html}{\out{&phi;}}{\eqn{\phi}{phi}}
parameter of the AR(1) process, using an Accept-Reject algorithm
}

\description{
The function inferf is used to create a sample from the posterior distribution
of \ifelse{html}{\out{&phi;}}{\eqn{\phi}{phi}}. The function uses the eq.10
(Tyralis and Koutsoyiannis 2014) to make inference on
\ifelse{html}{\out{&phi;}}{\eqn{\phi}{phi}} and an Accept-Reject algorithm (see
Robert and Casella 2004, Algorithm A.4).
}

\usage{
inferf(data, n, add = 0.001, minu = -0.999, maxu = 0.999)
}

\arguments{
  \item{data}{time series data}
  \item{n}{The size of the simulated sample}
  \item{add}{A number added to the maximum value of the natural logarithm of
  eq.10, to avoid bugs of the Accept-Reject algorithm due to computation errors}
  \item{minu}{A lower bound to the parameter
  \ifelse{html}{\out{&phi;}}{\eqn{\phi}{phi}}}
  \item{maxu}{An upper bound to the parameter
  \ifelse{html}{\out{&phi;}}{\eqn{\phi}{phi}}}
}

\value{
Vector with the simulated sample.
}

\author{Hristos Tyralis}

\references{
Robert CP, Casella G (2004) \emph{Monte Carlo Statistical Methods}. New York:
Springer. \doi{10.1007/978-1-4757-4145-2}.

Tyralis H, Koutsoyiannis D (2014) A Bayesian statistical model for deriving
the predictive distribution of hydroclimatic variables.
\emph{Climate Dynamics} \bold{42(11-12)}:2867--2883.
\doi{10.1007/s00382-013-1804-y}.
}

\examples{
# Posterior distribution of the phi parameter of the AR(1) process for the Nile
# time series.

set.seed(12345)

samp.sim <- inferf(Nile,500)

hist(samp.sim, breaks = 20, main = expression(paste("Histogram of ",phi)),
xlab = expression(phi))
}

\keyword{models}