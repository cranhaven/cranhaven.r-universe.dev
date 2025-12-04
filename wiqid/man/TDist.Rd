\name{TDist}
\alias{TDist}
\alias{dt2}
\alias{pt2}
\alias{qt2}
\alias{rt2}
\alias{dt3}
\alias{pt3}
\alias{qt3}
\alias{rt3}
\title{
The Generalized Student's t Distribution
}
\description{
Density, distribution function, quantile function and random generation for the generalised t distribution with df degrees of freedom, using location and scale, or mean and sd. These are wrappers for \code{stats::dt}, etc.

Note: In earlier versions of \code{wiqid} the \code{scale} argument to \code{*t2} functions was incorrectly named \code{sd}; they are not the same. These function now give a warning with the correct value of the \code{sd}. New \code{*t3} functions do use \code{sd} which is only defined for \code{df > 2}.
}

\usage{
dt2(x, location, scale, df)
pt2(x, location, scale, df, lower.tail=TRUE, log.p=FALSE)
qt2(p, location, scale, df, lower.tail=TRUE, log.p=FALSE)
rt2(n, location, scale, df)

dt3(x, mean, sd, df)
pt3(x, mean, sd, df, lower.tail=TRUE, log.p=FALSE)
qt3(p, mean, sd, df, lower.tail=TRUE, log.p=FALSE)
rt3(n, mean, sd, df)
}
\arguments{
  \item{x}{
vector of parameter values
}
  \item{location}{
location of the t-distribution
}
  \item{mean}{
mean of the t-distribution
}
  \item{scale}{
scale parameter of the t-distribution
}
  \item{sd}{
standard deviation of the t-distribution, only defined for \code{df > 2}.
}
  \item{df}{
degrees of freedom
}
  \item{lower.tail}{
logical; if TRUE (default), cumulative probabilities up to x, otherwise, above x.
}
  \item{log.p}{
logical; if TRUE, probabilities p are given as log(p).
}
  \item{p}{
probability.
}
  \item{n}{
number of random draws required.
}
}
\value{
\code{dtx} gives the density, \code{ptx} gives the cumulative probability, \code{qtx} gives the quantile function, and \code{rtx} generates random deviates.
}
\author{
Mike Meredith
}

\seealso{
See the \pkg{stats} functions \code{\link{dt}}, \code{\link{pt}}, \code{\link{qt}}, \code{\link{rt}}.
}
\examples{
## Plot the t-distribution with varying sd and scale
require(graphics)
xx <- seq(-5, 15, length=201)
density <- dt3(xx, mean=5, sd=1, 4)
plot(xx, density, type='l', lwd=2, main="t-distribution with df = 4")
lines(xx, dt3(xx, 5, 2, 4), lwd=2, col=2)
lines(xx, dt3(xx, 5, 3, 4), lwd=2, col=3)
lines(xx, dt3(xx, 5, 4, 4), lwd=2, col=4)
legend('topleft', paste0("sd = ", 1:4), lwd=2, lty=1, col=1:4,
  bty='n')
lines(xx, dt2(xx, 5, 1, 4), lwd=2, lty=2, col=1)
lines(xx, dt2(xx, 5, 2, 4), lwd=2, lty=2, col=2)
lines(xx, dt2(xx, 5, 3, 4), lwd=2, lty=2, col=3)
lines(xx, dt2(xx, 5, 4, 4), lwd=2, lty=2, col=4)
legend('topright', paste0("scale = ", 1:4), lwd=2, lty=2, col=1:4,
  bty='n')

# Generate random numbers
rand2 <- rt2(1e6, location=5, scale=2, df=4)
mean(rand2)
sd(rand2)  # about 2.83
rand3 <- rt3(1e6, mean=5, sd=2, df=4)
mean(rand3)
sd(rand3)  # close to 2

# check pt* and qt*
prob <- pt2(x=7, location=5, scale=3, df=4)
qt2(p=prob, location=5, scale=3, df=4)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{"distribution"}