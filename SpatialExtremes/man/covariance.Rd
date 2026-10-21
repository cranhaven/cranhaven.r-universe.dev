\name{covariance}
\alias{covariance}
\title{Defines and computes covariance functions}
\description{This function defines and computes several covariance
  function either from a fitted ``max-stable'' model; either by
  specifying directly the covariance parameters.
}
\usage{
covariance(fitted, nugget, sill, range, smooth, smooth2 = NULL, cov.mod =
"whitmat", plot = TRUE, dist, xlab, ylab, col = 1, ...)
}
\arguments{
  \item{fitted}{An object of class ``maxstab''. Most often this will be
    the output of the \code{\link{fitmaxstab}} function. May be missing
    if \code{sill}, \code{range}, \code{smooth} and \code{cov.mod} are
    given.}
  \item{nugget,sill,range,smooth,smooth2}{The nugget, sill, scale and smooth parameters
    for the covariance function. May be missing if \code{fitted} is
    given.}
  \item{cov.mod}{Character string. The name of the covariance
    model. Must be one of "whitmat", "cauchy", "powexp", "bessel" or
    "caugen" for the Whittle-Matern, Cauchy, Powered Exponential, Bessel
    and Generalized Cauchy models. May be missing if \code{fitted} is
    given.}
  \item{plot}{Logical. If \code{TRUE} (default) the covariance function
    is plotted.}
  \item{dist}{A numeric vector corresponding to the distance at which
    the covariance function should be evaluated. May be missing.}
  \item{xlab,ylab}{The x-axis and y-axis labels. May be missing.}
  \item{col}{The color to be used for the plot.}
  \item{\dots}{Several option to be passed to the \code{\link{plot}}
    function.}
}
\value{This function returns the covariance function. Eventually, if
  \code{dist} is given, the covariance function is computed for each
  distance given by \code{dist}. If \code{plot = TRUE}, the covariance
  function is plotted.}
\details{
  Currently, four covariance functions are defined: the Whittle-Matern,
  powered exponential (also known as "stable"), Cauchy and Bessel
  models. These covariance functions are defined as follows for \eqn{h >
    0}{h > 0}

  \describe{
    \item{Whittle-Matern}{\eqn{\gamma(h) = \sigma
	\frac{2^{1-\kappa}}{\Gamma(\kappa)} \left(\frac{h}{\lambda}
	\right)^{\kappa} K_{\kappa}\left(\frac{h}{\lambda} \right)}{\gamma(h) =
	\sigma  2^(1-\kappa) / \Gamma(\kappa)  (h/\lambda)^\kappa  K_\kappa(h
    / \lambda)}}
    \item{Powered Exponential}{\eqn{\gamma(h) = \sigma \exp \left[-
	\left(\frac{h}{\lambda} \right)^{\kappa} \right]}{\gamma(h) = \sigma
    exp[-(h/\lambda)^\kappa]}}
    \item{Cauchy}{\eqn{\gamma(h) = \sigma \left[1 + \left(\frac{h}{\lambda}
	\right)^2 \right]^{-\kappa}}{\gamma(h) = \sigma [1 +
    (h/\lambda)^2]^(-\kappa)}}
    \item{Bessel}{\eqn{\gamma(h) = \sigma \left(\frac{2
	  \lambda}{h}\right)^{\kappa} \Gamma(\kappa + 1)
	J_{\kappa}\left(\frac{h}{\lambda} \right)}{\gamma(h) = \sigma (2 \lambda /
	h)^(\kappa) Gamma(\kappa + 1) J_\kappa(h / \lambda)}}
    \item{Generalized Cauchy}{\eqn{\gamma(h) = \sigma \left\{1 +
    \left(\frac{h}{\lambda} \right)^{\kappa_2} \right\}^{-\kappa /
    \kappa_2}}{\gamma(h) = \sigma [1 + (h /
    \lambda)^\kappa_2]^(-\kappa / \kappa_2)}}
  }
  where \eqn{\sigma}{\sigma}, \eqn{\lambda}{\lambda} and
  \eqn{\kappa}{\kappa} are the sill, the range and shape parameters,
  \eqn{\Gamma}{\Gamma} is the gamma function,
  \eqn{K_{\kappa}}{K_\kappa} and  \eqn{J_\kappa}{J_\kappa} are both
  modified Bessel functions of order \eqn{\kappa}{\kappa}. In addition
  a nugget effect can be set that is there is a jump at the origin,
  i.e., \eqn{\gamma(o) = \nu + \sigma}{\gamma(o) = \nu + \sigma}, where
  \eqn{\nu}{\nu} is the nugget effect.
}
\examples{
## 1- Calling covariance using fixed covariance parameters
covariance(nugget = 0, sill = 1, range = 1, smooth = 0.5, cov.mod = "whitmat")
covariance(nugget = 0, sill = 0.5, range = 1, smooth = 0.5, cov.mod = "whitmat",
  dist = seq(0,5, 0.2), plot = FALSE)

## 2- Calling covariance from a fitted model
##Define the coordinate of each location
n.site <- 30
locations <- matrix(runif(2*n.site, 0, 10), ncol = 2)
colnames(locations) <- c("lon", "lat")

##Simulate a max-stable process - with unit Frechet margins
data <- rmaxstab(30, locations, cov.mod = "whitmat", nugget = 0, range =
3, smooth = 1)

##Fit a max-stable model
fitted <- fitmaxstab(data, locations, "whitmat", nugget = 0)
covariance(fitted, ylim = c(0, 1))
covariance(nugget = 0, sill = 1, range = 3, smooth = 1, cov.mod = "whitmat", add =
TRUE, col = 3)
title("Whittle-Matern covariance function")
legend("topright", c("Theo.", "Fitted"), lty = 1, col = c(3,1), inset =
.05)
}
\author{Mathieu Ribatet}
\keyword{htest}