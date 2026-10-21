\name{madogram}
\alias{madogram}
\title{Computes madograms}
\description{Computes the madogram for max-stable processes.}
\usage{
madogram(data, coord, fitted, n.bins, gev.param = c(0, 1, 0), which =
c("mado", "ext"), xlab, ylab, col = c(1, 2), angles = NULL, marge =
"emp", add = FALSE, xlim = c(0, max(dist)), ...)
}
\arguments{
  \item{data}{A matrix representing the data. Each column corresponds to
    one location.}
  \item{coord}{A matrix that gives the coordinates of each
    location. Each row corresponds to one location.}
  \item{fitted}{An object of class maxstab - usually the output of the
    \code{\link{fitmaxstab}} function. May be missing.}
  \item{n.bins}{The number of bins to be used. If missing, pairwise
    madogram estimates will be computed.}
  \item{gev.param}{Numeric vector of length 3 specifying the location,
    scale and shape parameters for the GEV.}
  \item{which}{A character vector of maximum size 2. It specifies if the
    madogram and/or the extremal coefficient functions have to be
    plotted.}
  \item{xlab,ylab}{The x-axis and y-axis labels. May be missing. Note
    that \code{ylab} must have the same length has \code{which}.}
  \item{col}{The colors used for the points and optionnaly for the
    fitted curve.}
  \item{angles}{A numeric vector. A partition of the interval
    \eqn{(0, \pi)}{(0, \pi)} to help detecting anisotropy.}
  \item{marge}{Character string. If 'emp', the observation are first
    transformed to the unit Frechet scale by using the empirical CDF. If
    'mle' (default), maximum likelihood estimates are used.}
  \item{add}{Logical. If \code{TRUE}, the plot is added to the current
    figure; otherwhise (default) a new plot is computed.}
  \item{xlim}{A numeric vector of length 2 specifying the x coordinate
    range.} 
  \item{\dots}{Additional options to be passed to the \code{\link{plot}}
    function.} 
}
\value{
  A graphic and (invisibly) a matrix with the lag distances, the
  madogram and extremal coefficient estimates. 
}
\details{
  Let \eqn{Z(x)}{Z(x)} be a stationary process. The madogram is defined
  as follows:

  \deqn{\nu(h) = \frac{1}{2}\mbox{E}\left[|Z(x+h) - Z(x)|
    \right]}{\nu(h) = 0.5 * E[|Z(x+h) - Z(x)|]}

  If now \eqn{Z(x)}{Z(x)} is a stationary max-stable random field with
  GEV marginals. Provided the GEV shape parameter \eqn{\xi}{\xi} is such
  that \eqn{\xi < 1}{\xi <1}. The extremal coefficient
  \eqn{\theta(h)}{\theta(h)} satisfies:
  \deqn{\theta(h) =
    \left\{
    \begin{array}{ll}
    u_\beta \left(\mu +  \frac{\nu(h)}{\Gamma(1 - \xi)} \right), &\xi
    \neq 0\\
    \exp\left(\frac{\nu(h)}{\sigma}\right), &\xi = 0
    \end{array}
    \right.}{u_\beta (\mu + \nu(h) / \Gamma(1 - \xi)), if \xi < 1,
  exp(\nu(h)/\sigma), otherwise}
  where \eqn{\Gamma}{\Gamma} is the gamma function and
  \eqn{u_\beta}{u_\beta} is defined as follows: 

  \deqn{u_\beta(u) = \left(1 + \xi \frac{u - \mu}{\sigma}
    \right)_+^{1/\xi}}{(1 + \xi (u - \mu) / \sigma )_+^{1/\xi}} and
    \eqn{\beta = (\mu, \sigma, \xi)}{\beta= (\mu, \sigma, \xi)}, i.e,
    the vector of the GEV parameters.
}
\references{
  Cooley, D., Naveau, P. and Poncet, P. (2006) Variograms for
  spatial max-stable random fields. \emph{Dependence in Probability and
    Statistics}, 373--390.
}
\seealso{
  \code{\link{fmadogram}}, \code{\link{lmadogram}}
}
\examples{
n.site <- 15
locations <- matrix(runif(2*n.site, 0, 10), ncol = 2)
colnames(locations) <- c("lon", "lat")

##Simulate a max-stable process - with unit Frechet margins
data <- rmaxstab(40, locations, cov.mod = "whitmat", nugget = 0, range = 1,
smooth = 2)

##Compute the madogram
madogram(data, locations)

##Compare the madogram with a fitted max-stable model
fitted <- fitmaxstab(data, locations, "whitmat", nugget = 0)
madogram(fitted = fitted, which = "ext")
}
\author{Mathieu Ribatet}
\keyword{hplot}
