\name{extcoeff}
\alias{extcoeff}
\title{Plots the extremal coefficient}
\description{
  Plots the extremal coefficient evolution as the coordinates evolves.
}
\usage{
extcoeff(fitted, cov.mod, param, n = 200, xlab, ylab, \dots)
}
\arguments{
  \item{fitted}{A object of class "\code{maxstab}". Most often, it will
    be the output of the function \code{\link{fitmaxstab}}. If missing,
    then \code{cov.mod} and \code{param} should be supplied.}
  \item{cov.mod}{A character string corresponding the the covariance
    model in the max-stable representation. Must be one of "gauss" for
    the Smith's model; or "whitmat", "cauchy" or "powexp" for
    the Whittle-Matern, the Cauchy and the Powered Exponential
    covariance family with the Schlather's model. May be missing if
    \code{fitted} is given.}
  \item{param}{Numeric vector of length 3. The parameters for the
    Smith's or Schlather model - i.e. c(cov11, cov12, cov22) or c(nugget,
    range, smooth). Please respect this order.}
  \item{n}{Numeric. \code{n^2} corresponds to the total number of
    estimated extremal coefficients for the contour plot.}
  \item{xlab,ylab}{The x-axis and y-axis labels. May be missing.}
  \item{\dots}{Several options to be passed to the \code{\link{contour}}
    function.}
}
\value{
  A plot.
}
\seealso{
  \code{\link{fitmaxstab}}
}
\examples{
## 1- Random field generation
n.site <- 30
locations <- matrix(runif(2*n.site, 0, 10), ncol = 2)
colnames(locations) <- c("lon", "lat")

data <- rmaxstab(60, locations, cov.mod = "whitmat", nugget = 0, range =
3, smooth = 1)

## 2- Fit a max-stable processes
schlather <- fitmaxstab(data, locations, "whitmat", nugget = 0)

## 3- Plot the extremal coefficient
extcoeff(schlather)
}
\author{Mathieu Ribatet}
\keyword{hplot}
