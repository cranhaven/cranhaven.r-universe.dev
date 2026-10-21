\name{rgp}
\alias{rgp}
\title{
  Gaussian Random Fields Simulation
}
\description{
This functions generates gaussian random fields.
}
\usage{
rgp(n, coord, cov.mod = "powexp", mean = 0, nugget = 0, sill = 1, range
= 1, smooth = 1, grid = FALSE, control = list())
}
\arguments{
  \item{n}{Integer. The number of replications.}
  \item{coord}{The locations coordinates for which the gaussian process
    is observed.}
  \item{cov.mod}{Character string. The covariance model used. Must be
    one of "whitmat", "cauchy", "powexp" of "cauchy". See the
    \code{\link{covariance}} function.}
  \item{mean}{Numeric. The mean of the gaussian random field.}
  \item{nugget}{Numeric. The nugget of the gaussian random field.}
  \item{sill}{Numeric. The sill parameter in the covariance function.}
  \item{range}{Numeric. The range parameter in the covariance function.}
  \item{smooth}{Numeric. The smooth parameter in the covariance function.}
  \item{grid}{Logical. Does \code{coord} defines a grid?}
  \item{control}{A named list with arguments 'nlines' (number of lines
    of the TBM simulation) and 'method' the name of the simulation
    method - must be one of 'exact', 'tbm' or 'circ'. If 'method' is
    \code{NULL} (default), the function tries to find the most
    appropriate simulation technique. If 'nlines' is \code{NULL} it is
    set to 1000.}
}
\value{
  A matrix or an array containing the random field replicates.
}
\author{
  Mathieu Ribatet
}
\seealso{
\code{link{rmaxstab}}
}
\examples{
x <- y <- seq(0, 20, length = 100)
coord <- cbind(x, y)
gp <- rgp(1, coord, cov.mod = "whitmat", grid = TRUE)
filled.contour(x, y, gp, color.palette = terrain.colors)
}
\keyword{distribution}
