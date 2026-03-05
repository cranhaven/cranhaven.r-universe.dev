\name{hglogis}
\title{Log Hazard Function for a Generalized Logistic Process}
\usage{
hglogis(y, m, s, f)
}
\alias{hglogis}
\description{
These functions provide information about the generalized logistic
distribution with location parameter equal to \code{m}, dispersion equal
to \code{s}, and family parameter equal to \code{f}: log hazard.
(See `rmutil` for the d/p/q/r boxcox functions density,
cumulative distribution, quantiles, and random generation).

The generalized logistic distribution has density
\deqn{
f(y) =
\frac{\nu \sqrt{3} \exp(-\sqrt{3} (y-\mu)/(\sigma \pi))}{
\sigma \pi (1+\exp(-\sqrt{3} (y-\mu)/(\sigma \pi)))^{\nu+1}}}{
f(y) = f sqrt(3) exp(-sqrt(3) (y-m)/(s pi))/
(s pi (1+exp(-sqrt(3) (y-m)/(s pi)))^(f+1))}
	
where \eqn{\mu}{m} is the location parameter of the distribution,
\eqn{\sigma}{s} is the dispersion, and \eqn{\nu}{f} is the family
parameter.

\eqn{\nu=1}{f=1} gives a logistic distribution.
}
\arguments{
\item{y}{vector of responses.}
\item{m}{vector of location parameters.}
\item{s}{vector of dispersion parameters.}
\item{f}{vector of family parameters.}
}
\author{J.K. Lindsey}
\seealso{
\code{\link{dlogis}} for the logistic distribution.
}
\examples{
hglogis(5, 5, 1, 2)
}
\keyword{distribution}
