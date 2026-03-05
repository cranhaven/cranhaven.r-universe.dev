\name{hskewlaplace}
\title{Log Hazard Function for a Skew Laplace Process}
\usage{
hskewlaplace(y, m=0, s=1, f=1)
}
\alias{hskewlaplace}
\description{
These functions provide information about the skew Laplace distribution
with location parameter equal to \code{m}, dispersion equal to
\code{s}, and skew equal to \code{f}: log hazard.
(See `rmutil` for the d/p/q/r boxcox functions density,
cumulative distribution, quantiles, and random generation).
For \code{f=1}, this is an ordinary (symmetric) Laplace distribution.

The skew Laplace distribution has density
\deqn{
f(y) = \frac{\nu\exp(-\nu(y-\mu)/\sigma)}{(1+\nu^2)\sigma}}{
f(y) = f*exp(-f*(y-m)/s)/((1+f^2)*s)}
if  \eqn{y\ge\mu}{y>=m} and else
\deqn{
f(y) = \frac{\nu\exp((y-\mu)/(\nu\sigma))}{(1+\nu^2)\sigma}}{
f(y) = f*exp((y-m)/(f*s))/((1+f^2)*s)}
where \eqn{\mu}{m} is the location parameter of the distribution,
\eqn{\sigma}{s} is the dispersion, and \eqn{\nu}{f} is the skew.

The mean is given by \eqn{\mu+\frac{\sigma(1-\nu^2)}{\sqrt{2}\nu}}{m
+ (s * (1 - f^2)) / (sqrt(2) * f)}
and the variance by  \eqn{\frac{\sigma^2(1+\nu^4)}{2\nu^2}}{(s^2
* (1 + f^4)) / (2 * f^2)}.

Note that this parametrization of the skew (family) parameter is
different than that used for the multivariate skew Laplace
distribution in 'growth::elliptic'.
}
\arguments{
\item{y}{vector of responses.}
\item{m}{vector of location parameters.}
\item{s}{vector of dispersion parameters.}
\item{f}{vector of skew parameters.}
}
\author{J.K. Lindsey}
\seealso{
\code{\link{dexp}} for the exponential distribution,
\code{\link{dcauchy}} for the Cauchy distribution, and
\code{\link[rmutil]{dlaplace}} for the Laplace distribution.
}
\examples{
hskewlaplace(5, 2, 1, 0.5)
}
\keyword{distribution}
