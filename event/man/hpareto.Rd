\name{hpareto}
\title{Log Hazard Function for a Pareto Process}
\usage{
hpareto(y, m, s)
}
\alias{hpareto}
\description{
These functions provide information about the Pareto distribution
with location parameter equal to \code{m} and dispersion equal to
\code{s}: log hazard.
(See `rmutil` for the d/p/q/r boxcox functions density,
cumulative distribution, quantiles, and random generation).

The Pareto distribution has density
\deqn{
f(y) = \frac{\sigma }{\mu (\sigma-1)(1 + y/(\mu (\sigma-1)))^{\sigma+1}}}{
f(y) = s (1 + y/(m (s-1)))^(-s-1)/(m (s-1))}
where \eqn{\mu}{m} is the mean parameter of the distribution and
\eqn{\sigma}{s} is the dispersion.

This distribution can be obtained as a mixture distribution from the
exponential distribution using a gamma mixing distribution.
}
\arguments{
\item{y}{vector of responses.}
\item{m}{vector of location parameters.}
\item{s}{vector of dispersion parameters.}
}
\author{J.K. Lindsey}
\seealso{
\code{\link{dexp}} for the exponential distribution.
}
\examples{
hpareto(5, 2, 2)
}
\keyword{distribution}
