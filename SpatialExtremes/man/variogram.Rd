\name{variogram}
\alias{variogram}
\title{
  Empirical variogram
}
\description{
This function computes the empirical variogram.
}
\usage{
variogram(data, coord, n.bins, xlab, ylab, angles = NULL, add = FALSE,
xlim = c(0, max(dist)), ...)
}
\arguments{
  \item{data}{A matrix representing the data. Each column corresponds to
    one location.}
  \item{coord}{A matrix that gives the coordinates of each
    location. Each row corresponds to one location.}
  \item{n.bins}{The number of bins to be used. If missing, pairwise
    madogram estimates will be computed.}
  \item{xlab,ylab}{The x-axis and y-axis labels. May be missing. Note
    that \code{ylab} must have the same length has \code{which}.}
  \item{angles}{A numeric vector. A partition of the interval
    \eqn{(0, \pi)}{(0, \pi)} to help detecting anisotropy.}
  \item{add}{Logical. If \code{TRUE}, the plot is added to the current
    figure; otherwhise (default) a new plot is computed.}
  \item{xlim}{A numeric vector of length 2 specifying the x coordinate
    range.} 
  \item{\dots}{Additional options to be passed to the \code{\link{plot}}
    function.}
}
\value{
  A graphic and (invisibly) a matrix with the lag distances and the
  empirical variogram estimates.
}
\author{Mathieu Ribatet}
\seealso{
\code{\link{fmadogram}}, \code{\link{lmadogram}}
}
\examples{
n.site <- 20
n.obs <- 100
coord <- matrix(runif(2 * n.site, 0, 10), ncol = 2)
data <- rgp(n.obs, coord, "powexp", sill = 2, range = 3, smooth = 1)
variogram(data, coord)
}
\keyword{hplot}
