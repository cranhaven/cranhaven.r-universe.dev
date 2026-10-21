\name{rcopula}
\alias{rcopula}
\title{Simulation from copula based models with unit Frechet margins}
\description{
  This function generates realisations from the Gaussian and Student
  copula with unit Frechet margins.
}
\usage{
rcopula(n, coord, copula = "gaussian", cov.mod = "whitmat", grid =
FALSE, control = list(), nugget = 0, range = 1, smooth = 1, DoF = 1)
}
\arguments{
  \item{n}{Integer. The number of observations.}
  \item{coord}{A vector or matrix that gives the coordinates of each
    location. Each row corresponds to one location - if any}
  \item{copula}{A character string that specifies the copula to be used,
    i.e., "gaussian" or "student".}
  \item{cov.mod}{A character string that gives the correlation function
    family to be used. This must be one of "whitmat", "cauchy", "powexp"
    and "bessel" for the Whittle-Matern, the cauchy, the powered
    exponential and the bessel correlation functions.}
  \item{grid}{Logical. Does the coordinates represent grid points?}
  \item{control}{A named list that control the simulation of the
    gaussian process --- see \code{\link{rgp}}.}
  \item{nugget, range, smooth, DoF}{Numerics. The parameters of the
    copula.}
}
\value{
  A matrix containing observations from the required max-stable
  model. Each column represents one stations. If \code{grid = TRUE}, the
  function returns an array of dimension nrow(coord) x nrow(coord) x n.
}


\references{
  Demarta, S. and McNeil, A. J. (2005) The t Copula and Related Copulas
  \emph{International Statistical Review} \bold{73}:1, 111--129.

  Davison, A. C., Padoan, S. A. and Ribatet, M. (2010) Statistical
  Modelling of Spatial Extremes \emph{Submitted to Statistical
  Science}.
}

\author{
Mathieu Ribatet
}
\seealso{
  \code{\link{fitcopula}}, \code{\link{rmaxstab}}, \code{\link{rmaxlin}}
}
\examples{
n.site <- 25
n.obs <- 50

coord <- matrix(runif(2 * n.site, 0, 10), ncol = 2)

data1 <- rcopula(n.obs, coord, "student", "whitmat", range = 3, DoF = 3)

x <- y <- seq(0, 10, length = 100)
data2 <- rcopula(1, cbind(x, y), "gaussian", "whitmat", range = 3, grid
= TRUE)
image(x, y, log(data2), col = rainbow(64))
}
\keyword{distribution}
