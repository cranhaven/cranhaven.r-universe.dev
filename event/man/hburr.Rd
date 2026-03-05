\name{hburr}
\title{Log Hazard Function for a Burr Process}
\usage{
hburr(y, m, s, f)
}
\alias{hburr}
\description{
These functions provide information about the Burr distribution with
location parameter equal to \code{m}, dispersion equal to
\code{s}, and family parameter equal to \code{f}:  log hazard.
(See `rmutil` for the d/p/q/r boxcox functions density,
cumulative distribution, quantiles, and random generation).

The Burr distribution has density
\deqn{
f(y) = \frac{\nu \sigma (y / \mu)^{\sigma-1}}
{\mu (1+(y/\mu)^\sigma)^{\nu+1}}}{
f(y) = f s (y/m)^(s-1)/(m (1+(y/m)^s)^(f+1))}
where \eqn{\mu}{m} is the location parameter of the distribution,
\eqn{\sigma}{s} is the dispersion, and \eqn{\nu}{f} is the family
parameter.
}
\arguments{
\item{y}{vector of responses.}
\item{m}{vector of location parameters.}
\item{s}{vector of dispersion parameters.}
\item{f}{vector of family parameters.}
}
\author{J.K. Lindsey}
\examples{
hburr(2, 5, 1, 2)
}
\keyword{distribution}
