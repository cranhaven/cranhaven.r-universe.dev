\name{hlaplace}
\title{Log Hazard Function for a Laplace Process}
\usage{
hlaplace(y, m=0, s=1)
}
\alias{hlaplace}
\description{
These functions provide information about the Laplace distribution
with location parameter equal to \code{m} and dispersion equal to
\code{s}: log hazard.
(See `rmutil` for the d/p/q/r boxcox functions density,
cumulative distribution, quantiles, and random generation).

The Laplace distribution has density
\deqn{
f(y) = \frac{\exp(-abs(y-\mu)/\sigma)}{(2\sigma)}}{
f(y) = exp(-abs(y-m)/s)/(2*s)}
where \eqn{\mu}{m} is the location parameter of the distribution and
\eqn{\sigma}{s} is the dispersion.
}
\arguments{
\item{y}{vector of responses.}
\item{m}{vector of location parameters.}
\item{s}{vector of dispersion parameters.}
}
\author{J.K. Lindsey}
\seealso{
\code{\link{dexp}} for the exponential distribution and
\code{\link{dcauchy}} for the Cauchy distribution.
}
\examples{
hlaplace(5, 2, 1)
}
\keyword{distribution}