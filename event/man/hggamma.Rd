\name{hggamma}
\title{Log Hazard Function for a Generalized Gamma Process}
\usage{
hggamma(y, s, m, f)
}
\alias{hggamma}
\description{
These functions provide information about the generalized gamma
distribution with scale parameter equal to \code{m}, shape equal
to \code{s}, and family parameter equal to \code{f}: log hazard.
(See `rmutil` for the d/p/q/r boxcox functions density,
cumulative distribution, quantiles, and random generation).

The generalized gamma distribution has density
\deqn{
  f(y) = \frac{\nu y^{\nu-1}}
  {(\mu/\sigma)^{\nu\sigma} Gamma(\sigma)} y^{\nu(\sigma-1)}
  \exp(-(y \sigma/\mu)^\nu)}{
  f(y) = fy^(f-1)/((m/s)^(fs) Gamma(s)) y^(f(s-1)) exp(-(y s/m)^f)}
	
where \eqn{\mu}{m} is the scale parameter of the distribution,
\eqn{\sigma}{s} is the shape, and \eqn{\nu}{f} is the family
parameter.

\eqn{\nu=1}{f=1} yields a gamma distribution, \eqn{\sigma=1}{s=1} a
Weibull distribution, and \eqn{\sigma=\infty}{s=infinity} a
log normal distribution.
}
\arguments{
\item{y}{vector of responses.}
\item{m}{vector of location parameters.}
\item{s}{vector of dispersion parameters.}
\item{f}{vector of family parameters.}
}
\author{J.K. Lindsey}
\seealso{
\code{\link{dgamma}} for the gamma distribution,
\code{\link{dweibull}} for the Weibull distribution, \code{\link{dlnorm}}
for the log normal distribution.
}
\examples{
hggamma(2, 5, 4, 2)
}
\keyword{distribution}
