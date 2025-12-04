\name{densityFolded}
\alias{densityFolded}

\title{
Folded kernel density estimation
}
\description{
Parameters are often constrained to be greater than zero (eg, standard deviation) or within the range (0, 1) (eg, probabilities), but the function \code{density} often returns non-zero densities outside these ranges. Simple truncation does not work, as the area under the curve is < 1. The function \code{densityFolded} attempts to identify these constraints and gives an appropriate density.

If \code{x} is a matrix, detection of constraints and selection of bandwidth is applied to the pooled values, but a separate density curve is fitted to each column of the matrix.
}
\usage{
densityFolded(x, bw = "nrd0", adjust = 1, from=NA, to=NA, ...)
}
\arguments{
 \item{x}{
a numeric vector or matrix from which the estimate is to be computed; missing values not allowed. %%%% why no missing values? %%%%
}
  \item{bw}{
the smoothing bandwidth to be used; see \code{\link{density}} for details.
}
  \item{adjust}{
the bandwidth used is actually \code{adjust*bw}.
}
  \item{from, to}{
the lower and upper ends of the grid at which the density is to be estimated; if NA, range will cover the values in x; ignored and replaced with 0 or 1 if a constraint is detected.
}
  \item{...}{
other arguments passed to \code{\link{density}}.
}
}

\value{
Returns a list containing the following components:
\describe{
  \item{x }{the n coordinates of the points where the density is estimated.}
  \item{y	}{a vector or matrix with the estimated density values.}
  \item{bw }{the bandwidth used.}
  \item{n	}{the sample size after elimination of missing values.}
  \item{call }{the call which produced the result.}
  \item{data.name	}{the deparsed name of the x argument.}
}

If \code{y} is a vector, the output will have class \code{\link{density}}.
}

\author{
Mike Meredith
}
\examples{
require(graphics)
oldpar <- par(mfrow=2:1)

x1 <- rnorm(1e4)            # no constraint on x1
plot(density(x1))
plot(densityFolded(x1))     # no difference

x2 <- abs(rnorm(1e4))       # x2 >= 0, with mode at 0
plot(density(x2))           # density > 0 when x2 < 0, mode around 0.2
abline(v=0, col='grey')
plot(densityFolded(x2))     # mode plotted correctly
abline(v=0, col='grey')

x3 <- rbeta(1e4, 1.5, 1.5)  # 0 <= x3 <= 1
plot(density(x3))           # density > 0 when x2 < 0 and x2 > 1
abline(v=0:1, col='grey')
plot(densityFolded(x3))
abline(v=0:1, col='grey')

x4 <- rbeta(1e4, 1.5, 0.9)  # 0 <= x4 <= 1, with mode at 1
plot(density(x4))           # mode appears to be around 0.95
abline(v=0:1, col='grey')
plot(densityFolded(x4))     # mode plotted correctly
abline(v=0:1, col='grey')

# Try with a matrix
x5 <- cbind(rbeta(1e4, 2,2), rbeta(1e4, 2,3), rbeta(1e4, 3,2))
plot(density(x5))
tmp <- densityFolded(x5)
with(tmp, matplot(x, y, type='l'))

par(oldpar)
}
