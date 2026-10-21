\name{kriging}
\alias{kriging}
\title{
Simple kriging interpolation
}
\description{
This function interpolates a zero mean Gaussian random field using the
simple kriging predictor.
}
\usage{
kriging(data, data.coord, krig.coord, cov.mod = "whitmat", sill, range,
smooth, smooth2 = NULL, grid = FALSE, only.weights = FALSE)
}
\arguments{
  \item{data}{A numeric vector or matrix. If \code{data} is a matrix
    then the simple kriging predictor is given for each realisation,
    i.e., each row of \code{data}.}
  \item{data.coord}{A numeric vector or matrix specifying the
    coordinates of the observed data. If \code{data.coord} is a matrix,
    each row must corresponds to one location.}
  \item{krig.coord}{A numeric vector or matrix specifying the
    coordinates where the kriging predictor has to be computed. If
    \code{krig.coord} is a matrix, each row must correspond to one
    location.}
  \item{cov.mod}{A character string specifying the covariance function
    family. Must be one of "whitmat", "powexp", "cauchy", "bessel" or
    "caugen" for the Whittle-Matern, the powered exponential, the
    Cauchy, the Bessel or the generalized Cauchy covariance families.}
  \item{sill,range,smooth,smooth2}{Numerics specifiying the sill, range,
    smooth and, if any, the second smooth parameters of the covariance
    function.} 
  \item{grid}{Logical. Does \code{krig.coord} specifies a grid?}
  \item{only.weights}{Logical. Should only the kriging weights be
    computed? If \code{FALSE}, the kriging predictor isn't computed.}
}
\value{
  A list with components
  \item{coord}{The coordinates where the kriging predictor has been
    computed;}
  \item{krig.est}{The kriging predictor estimates;}
  \item{grid}{Does \code{coord} define a grid?;}
  \item{weights}{A matrix giving the kriging weights: each column
    corresponds to one prediction location.}
}
\references{
Chiles, J.-P. and Delfiner, P. (1999) \emph{Geostatistics, Modeling
  Spatial Uncertainty} Wiley Series in Probability and Statistics.
}
\author{
Mathieu Ribatet
}
\seealso{
\code{\link{condrgp}}, \code{\link{rgp}}, \code{\link{covariance}}.
}
\examples{
## Kriging from a single realisation
n.site <- 50
n.pred <- 512

x.obs <- runif(n.site, -100, 100)
x.pred <- seq(-100, 100, length = n.pred)

data <- rgp(1, x.obs, "whitmat", sill = 1, range = 10, smooth = 0.75)

krig <- kriging(data, x.obs, x.pred, "whitmat", sill = 1, range = 10,
smooth = 0.75)

plot(krig$coord, krig$krig.est, type = "l", xlab = "x", ylab =
expression(hat(Y)(x)))
points(x.obs, data, col = 2, pch = 21, bg = 2)

## Kriging from several realisations
n.real <- 3
data <- rgp(n.real, x.obs, "whitmat", sill = 1, range = 10, smooth = 0.75)

krig <- kriging(data, x.obs, x.pred, "whitmat", sill = 1, range = 10,
smooth = 0.75)

matplot(krig$coord, t(krig$krig.est), type = "l", xlab = "x", ylab =
expression(hat(Y)(x)), lty = 1)
matpoints(x.obs, t(data), pch = 21, col = 1:n.real, bg = 1:n.real)
title("Three kriging predictors in one shot")

## Two dimensional kriging on a grid
x.obs <- matrix(runif(2 * n.site, -100, 100), ncol = 2)
x <- y <- seq(-100, 100, length = 100)
x.pred <- cbind(x, y)

data <- rgp(1, x.obs, "whitmat", sill = 1, range = 10, smooth = 0.75)

krig <- kriging(data, x.obs, x.pred, "whitmat", sill = 1, range = 10,
smooth = 0.75, grid = TRUE)

z.lim <- range(c(data, krig$krig.est))
breaks <- seq(z.lim[1], z.lim[2], length = 65)
col <- heat.colors(64)
idx <- as.numeric(cut(data, breaks))

image(x, y, krig$krig.est, col = col, breaks = breaks)
points(x.obs, bg = col[idx], pch = 21)
## Note how the background colors of the above points matches the ones
## returned by the image function
}
\keyword{spatial}
\keyword{htest}
