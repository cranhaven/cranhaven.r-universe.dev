\name{fmadogram}
\alias{fmadogram}
\title{Computes the F-madogram}
\description{Computes the F-madogram for max-stable processes.}
\usage{
fmadogram(data, coord, fitted, n.bins, which = c("mado", "ext"), xlab,
ylab, col = c(1, 2), angles = NULL, marge = "emp", add = FALSE, xlim =
c(0, max(dist)), ...)
}
\arguments{
  \item{data}{A matrix representing the data. Each column corresponds to
    one location.}
  \item{coord}{A matrix that gives the coordinates of each
    location. Each row corresponds to one location.}
  \item{fitted}{An object of class maxstab - usually the output of the
    \code{\link{fitmaxstab}} function. May be missing.}
  \item{n.bins}{The number of bins to be used. If missing, pairwise
    F-madogram estimates will be computed.}
  \item{which}{A character vector of maximum size 2. It specifies if the
    madogram and/or the extremal coefficient functions have to be
    plotted.}
  \item{xlab,ylab}{The x-axis and y-axis labels. May be missing. Note
    that \code{ylab} must have the same length has \code{which}.}
  \item{col}{The colors used for the points and optionnaly the fitted
    curve.}
  \item{angles}{A numeric vector. A partition of the interval
    \eqn{(-\pi, \pi)}{(-\pi, \pi)} to help detecting anisotropy.}
  \item{marge}{Character string. If 'emp', the probabilities of non
    exceedances are estimated using the empirical CDF. If 'mle'
    (default), maximum likelihood estimates are used.}
  \item{add}{Logical. If \code{TRUE}, the plot is added to the current
    figure; otherwhise (default) a new plot is computed.}
  \item{xlim}{A numeric vector of length 2 specifying the x coordinate
    range.}
  \item{\dots}{Additional options to be passed to the \code{\link{plot}}
    function.} 
}
\value{
  A graphic and (invisibly) a matrix with the lag distances, the
  F-madogram  and extremal coefficient estimates. 
}
\details{
  Let \eqn{Z(x)}{Z(x)} be a stationary process. The F-madogram is
  defined as follows:

  \deqn{\nu(h) = \frac{1}{2}\mbox{E}\left[|F(Z(x+h)) - F(Z(x))|
    \right]}{\nu(h) = 0.5 * E[|F(Z(x+h)) - F(Z(x))|]}

  The extremal coefficient \eqn{\theta(h)}{\theta(h)} satisfies:

  \deqn{\theta(h) = \frac{1 + 2 \nu(h)}{1 - 2 \nu(h)}}{{1 + 2 \nu(h)} /
    {1 - 2 \nu(h)}} 
}
\references{
  Cooley, D., Naveau, P. and Poncet, P. (2006) Variograms for
  spatial max-stable random fields. \emph{Dependence in Probability and
    Statistics}, 373--390.
}
\seealso{
  \code{\link{madogram}}, \code{\link{lmadogram}}
}
\examples{
n.site <- 15
locations <- matrix(runif(2*n.site, 0, 10), ncol = 2)
colnames(locations) <- c("lon", "lat")

##Simulate a max-stable process - with unit Frechet margins
data <- rmaxstab(40, locations, cov.mod = "whitmat", nugget = 0, range = 1,
smooth = 2)

##Compute the F-madogram
fmadogram(data, locations)

##Compare the F-madogram with a fitted max-stable process
fitted <- fitmaxstab(data, locations, "whitmat", nugget = 0)
fmadogram(fitted = fitted, which = "ext")
}
\author{Mathieu Ribatet}
\keyword{hplot}
