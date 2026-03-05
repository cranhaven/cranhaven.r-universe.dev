\name{hgextval}
\title{	Log Hazard Function for an Extreme Value  Process}
\usage{
hgextval(y, s, m, f)
}
\alias{hgextval}
\description{
These functions provide information about the generalized extreme
value distribution with location parameter equal to \code{m}, dispersion
equal to \code{s}, and family parameter equal to \code{f}:log hazard.
(See `rmutil` for the d/p/q/r boxcox functions density,
cumulative distribution, quantiles, and random generation).

The generalized extreme value distribution has density
\deqn{
  f(y) =
  y^{\nu-1} \exp(y^\nu/\nu) \frac{\sigma}{\mu}
  \frac{\exp(y^\nu/\nu)}{\mu^{\sigma-1}/(1-I(\nu>0)+sign(\nu)
  exp(-\mu^-\sigma))}\exp(-(\exp(y^\nu\nu)/\mu)^\sigma)}{
  f(y) = y^(f-1) exp(y^f/f) (s/m) (exp(y^f/f)/m)^(s-1)
exp(-(exp(y^f/f)/m)^s)/(1-I(f>0)+sign(f) exp(-m^-s))}
	
where \eqn{\mu}{m} is the location parameter of the distribution,
\eqn{\sigma}{s} is the dispersion, \eqn{\nu}{f} is the family
parameter, \eqn{I()} is the indicator function, and \eqn{y>0}.

\eqn{\nu=1}{f=1} a truncated extreme value distribution.
}
\arguments{
\item{y}{vector of responses.}
\item{m}{vector of location parameters.}
\item{s}{vector of dispersion parameters.}
\item{f}{vector of family parameters.}
}
\author{J.K. Lindsey}

\seealso{
\code{\link{dweibull}} for the Weibull distribution.
}
\examples{
hgextval(1, 2, 1, 2)
}
\keyword{distribution}
