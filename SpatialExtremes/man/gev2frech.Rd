\name{gev2frech}
\alias{gev2frech}
\alias{frech2gev}
\title{Transforms GEV data to unit Frechet ones and vice versa}
\description{
  Transforms GEV data to unit Frechet ones and vice versa
}
\usage{
gev2frech(x, loc, scale, shape, emp = FALSE)
frech2gev(x, loc, scale, shape)
}
\arguments{
  \item{x}{The data to be transformed to unit Frechet or ordinary GEV
    margins}
  \item{loc, scale, shape}{The location, scale and shape parameters of
    the GEV.}
  \item{emp}{Logical. If \code{TRUE}, data are transformed to unit
    Frechet margins using the empirical CDF.}
}
\value{
  Returns a numeric vector with the same length of \code{x}
}
\details{
  If Y is a random variable with a GEV distribution with location
  \eqn{\mu}{\mu}, scale \eqn{\sigma}{\sigma} and shape
  \eqn{\xi}{\xi}. Then,

  \deqn{Z = \left[1 + \xi \frac{Y - \mu}{\sigma} \right]_+^{1/\xi}}{Z =
  [1 + \xi (Y - \mu) / \sigma]_+^(1/\xi)}
  is unit Frechet distributed - where \eqn{x_+ = \max(0, x)}{x_+ =
    max(0, x)}.

  If Z is a unit Frechet random variable. Then,

  \deqn{Y = \mu + \sigma \frac{Z_+^{\xi} - 1}{\xi}}{Y = \mu + \sigma
  (Z_+^(\xi) - 1) / \xi}
  is unit GEV distributed with location, scale and shape parameters
  equal to \eqn{\mu}{\mu}, \eqn{\sigma}{\sigma} and \eqn{\xi}{\xi}
  respectively.
}
\examples{
x <- c(2.2975896, 1.6448808, 1.3323833, -0.4464904, 2.2737603,
    -0.2581876, 9.5184398, -0.5899699, 0.4974283, -0.8152157)
y <- gev2frech(x, 1, 2, .2)
y
frech2gev(y, 1, 2, .2)
}
\author{Mathieu Ribatet}
\keyword{datagen}
