\name{lmadogram}
\alias{lmadogram}
\title{Computes the lambda-madogram}
\description{Computes the lambda-madogram for max-stable processes.}
\usage{
lmadogram(data, coord, n.bins, xlab, ylab, zlab, n.lambda = 11, marge =
"emp", col = terrain.colors(50, alpha = 0.5), theta = 90, phi = 20,
border = NA, ...)
}
\arguments{
  \item{data}{A matrix representing the data. Each column corresponds to
    one location.}
  \item{coord}{A matrix that gives the coordinates of each
    location. Each row corresponds to one location.}
  \item{n.bins}{The number of bins to be used. If missing, pairwise
    lambda-madogram estimates will be computed.}
  \item{xlab,ylab,zlab}{The x-axis, y-axis and z-axis labels. May be
    missing.}
  \item{n.lambda}{Integer giving the number of lambda values.}
  \item{marge}{Character string. If 'emp', probabilities of non
    exceedances are estimated using the empirical CDF. If 'mle'
    (default),  maximum likelihood estimates are used.}
  \item{col}{The colors used to emphasize the gradient of the
    lambda-madogram.}
  \item{theta,phi,border}{Options to be passed to the
    \code{\link{persp}} function.}
  \item{\dots}{Additional options to be passed to the \code{\link{persp}}
    function.} 
}
\value{
  A graphic and (invisibly) a matrix with the lag distances, the
  \eqn{\lambda}{\lambda}-madogram estimate.
}
\details{
  Let \eqn{Z(x)}{Z(x)} be a stationary process. The
  \eqn{\lambda}{\lambda}-madogram is defined as follows:

  \deqn{\nu_{\lambda}(h) = \frac{1}{2}\mbox{E}\left[|F^\lambda(Z(x+h)) -
    F^{1-\lambda}(Z(x))| \right]}{\nu_\lambda(h) = 0.5 *
    E[|F^\lambda(Z(x+h)) - F^(1-\lambda)(Z(x))|]}
}
\references{
  Naveau, P., Guillou, A., Cooley, D. and Diebolt, J. (2009) Modelling
  Pairwise Dependence of Maxima in Space. To appear in
    \emph{Biometrika}.
}
\seealso{
  \code{\link{madogram}}, \code{\link{fmadogram}}
}
\examples{
n.site <- 50
locations <- matrix(runif(2*n.site, 0, 10), ncol = 2)
colnames(locations) <- c("lon", "lat")

##Simulate a max-stable process - with unit Frechet margins
data <- rmaxstab(40, locations, cov.mod = "whitmat", nugget = 0, range = 1,
smooth = 2)

##Compute the lambda-madogram
lmadogram(data, locations, n.bins = 80)
}
\author{Mathieu Ribatet}
\keyword{hplot}
