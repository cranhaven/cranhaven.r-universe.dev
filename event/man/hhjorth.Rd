\name{hhjorth}
\title{Log Hazard Function for a Hjorth Process}
\usage{
hhjorth(y, m, s, f)
}
\alias{hhjorth}
\description{
These functions provide information about the Hjorth
distribution with location parameter equal to \code{m}, dispersion equal
to \code{s}, and family parameter equal to \code{f}: log hazard.
(See `rmutil` for the d/p/q/r boxcox functions density,
cumulative distribution, quantiles, and random generation).


The Hjorth distribution has density
\deqn{
f(y) = (1+\sigma y)^{-\nu/\sigma} \exp(-(y/\mu)^2/2)
(\frac{y}{\mu^2}+\frac{\nu}{1+\sigma y})}{
f(y) = (1+s y)^(-f/s) exp(-(y/m)^2/2) (y/m^2+f/(1+s y))}
	
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
hhjorth(5, 5, 5, 2)
}
\keyword{distribution}
