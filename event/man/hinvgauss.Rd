\name{hinvgauss}
\title{Log Hazard Function for a Inverse Gauss Process}
\usage{
hinvgauss(y, m, s)
}
\alias{hinvgauss}
\description{
These functions provide information about the inverse Gaussian
distribution with mean equal to \code{m} and dispersion equal to
\code{s}: log hazard.
(See `rmutil` for the d/p/q/r boxcox functions density,
cumulative distribution, quantiles, and random generation).

The inverse Gaussian distribution has density
\deqn{
f(y) =
\frac{1}{\sqrt{2\pi\sigma y^3}} e^{-(y-\mu)^2/(2 y \sigma m^2)}}{
f(y) = 1/sqrt(2 pi s y^3) e^-((y - m)^2/(2 y s m^2))}
where \eqn{\mu}{m} is the mean of the distribution and
\eqn{\sigma}{s} is the dispersion.
}
\arguments{
\item{y}{vector of responses.}
\item{m}{vector of means.}
\item{s}{vector of dispersion parameters.}
}
\author{J.K. Lindsey}
\seealso{
\code{\link{dnorm}} for the normal distribution and
\code{\link{dlnorm}} for the \emph{Log}normal distribution.
}
\examples{
hinvgauss(5, 5, 1)
}
\keyword{distribution}
