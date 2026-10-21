\name{print}
\alias{print}
\alias{print.pspline2}
\alias{print.maxstab}
\alias{print.copula}
\alias{print.spatgev}
\alias{print.latent}

\title{Printing objects of classes defined in the SpatialExtreme packages}
\description{
  Methods for printing objects of classes introduced by the
  SpatialExtremes package.
}
\usage{
\method{print}{pspline2}(x, \dots)
\method{print}{maxstab}(x, digits = max(3, getOption("digits") - 3), \dots)
\method{print}{copula}(x, digits = max(3, getOption("digits") - 3), \dots)
\method{print}{spatgev}(x, digits = max(3, getOption("digits") - 3), \dots)
\method{print}{latent}(x, digits = max(3, getOption("digits") - 3), \dots,
 level = 0.95)

}
\arguments{
  \item{x}{An object of class 'pspline', 'maxstab', 'copula', 'spatgev'
    or 'latent'. Most often, \code{x} will be the output of one of the
    following functions: \code{\link{rbpspline}}, \code{\link{fitmaxstab}},
    \code{\link{fitcopula}}, \code{\link{fitspatgev}} or
    \code{\link{latent}}.}
  \item{digits}{The number of digits to be printed.}
  \item{\dots}{Other options to be passed to the \code{\link{print}}
    function.}
  \item{level}{A numeric giving the significance level for the credible
    intervals---class 'latent' only.}
}
\value{
  Print several information on screen.
}
\examples{
##Define the coordinates of each location
n.site <- 30
coord <- matrix(5 + rnorm(2*n.site, sd = sqrt(2)), ncol = 2)
colnames(coord) <- c("lon", "lat")

##Simulate a max-stable process - with unit Frechet margins
data <- rmaxstab(30, coord, cov.mod = "whitmat", nugget = 0, range = 3,
smooth = 0.5)

## Printing max-stable objects
fit <- fitmaxstab(data, coord, "whitmat")
fit

## Printing spatial GEV objects
loc.form <- scale.form <- shape.form <- y ~ 1
fit <- fitspatgev(data, coord, loc.form, scale.form, shape.form)
fit
}
\author{Mathieu Ribatet}
\keyword{print}