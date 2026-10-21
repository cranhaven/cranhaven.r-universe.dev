\name{rmaxstab}
\alias{rmaxstab}

\title{Simulation of Max-Stable Random Fields}
\description{This function generates realisation from a max-stable
  random field.}

\usage{
rmaxstab(n, coord, cov.mod = "gauss", grid = FALSE, control =
list(), \dots)
}

\arguments{
  \item{n}{Integer. The number of observations.}
  \item{coord}{A vector or matrix that gives the coordinates of each
    location. Each row corresponds to one location - if any.}
  \item{cov.mod}{A character string that gives the max-stable
    model. This must be one of "gauss" for the Smith model, "brown" for
  the Brown--Resnick model,  or "whitmat", "cauchy", "powexp" and
  "bessel" for the Schlather model with the given  correlation
  family. If the latters are prefix by a "t", e.g., "twhitmat", this
  would corresponds to the extremal-t model.}
  \item{grid}{Logical. Does the coordinates represent grid points?}
  \item{control}{A named list with arguments 'nlines' (number of lines
    of the TBM simulation), 'method' the name of the simulation
    method - must be one of 'exact', 'tbm' or 'circ', and 'uBound' the
  uniform upper bound. See details.}
  \item{\dots}{The parameters of the max-stable model. See details.}
}
\details{
  Users must supply the parameters for the max-stable model. For the
  Schlather model, users should supply the "nugget", "range" and "smooth"
  parameter values. For the Smith model, if \code{coord} is univariate
  you must specify \code{var}, otherwise users should supply the
  covariance parameters i.e. parameters with names such as \code{cov11},
  \code{cov12}, \ldots For the extremal-t model, users should supply the
  "DoF", "nugget", "range" and "smooth" parameters. Finally for the
  Brown--Resnick model, users should supply the "range" and the "smooth"
  parameters.

  Here are the details for each allowed components of 'control'. If
  'method' is \code{NULL} (default), the function tries to find the 
  most appropriate simulation technique. Current simulation techniques
  are a direct approach, i.e. Cholesky decomposition of the covariance
  matrix, the turning bands and the circular embedding methods. If
  'nlines' is \code{NULL}, it is set to 1000. If 'uBound' is
  \code{NULL}, it is set to reasonable values - for example 3.5 for the
  Schlather model.
}
\value{
  A matrix containing observations from the required max-stable
  model. Each column represents one stations. If \code{grid = TRUE}, the
  function returns an array of dimension nrow(coord) x nrow(coord) x n.
}
\references{
  Schlather, M. (2002) Models for Stationary Max-Stable Random
  Fields. \emph{Extremes} \bold{5}:1,  33--44.
  
  Smith, R. L. (1990) Max-stable processes and spatial
  extremes. Unpublished manuscript.
}
\author{Mathieu Ribatet}
\seealso{\code{\link{fitmaxstab}}}
\examples{
## 1. Smith's model
set.seed(8)
x <- y <- seq(0, 10, length = 100)
coord <- cbind(x, y)
data <- rmaxstab(1, coord, "gauss", cov11 = 9/8, cov12 = 0, cov22 = 9/8,
  grid = TRUE)
##We change to unit Gumbel margins for visibility
filled.contour(x, y, log(data), color.palette = terrain.colors)

## 2. Schlather's model
data <- rmaxstab(1, coord, cov.mod = "powexp", nugget = 0, range = 3,
smooth = 1, grid = TRUE)
filled.contour(x, y, log(data), color.palette = terrain.colors)

## 3. Brown--Resnick's model **** Only available for non gridded points currently ****
data <- rmaxstab(1, x, cov.mod = "brown", range = 3, smooth = 0.5)
plot(x, log(data), type = "l")

## 4. Extremal-t model *** Very time consuming for 2d grids ***
data <- rmaxstab(1, x, "twhitmat", DoF = 4, nugget = 0, range = 3,
smooth = 0.7)
plot(x, log(data), type = "l")
}
\keyword{distribution}
