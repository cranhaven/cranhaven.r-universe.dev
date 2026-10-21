\name{fitcovmat}
\alias{fitcovmat}
\title{Estimates the covariance matrix for the Smith's model}
\description{
  Estimates the covariance matrix for the Smith's model using
  non-parametric estimates of the pairwise extremal coefficients.
}
\usage{
fitcovmat(data, coord, marge = "emp", iso = FALSE, control = list(),
..., start, weighted = TRUE)
}
\arguments{
  \item{data}{A matrix representing the data. Each column corresponds to
    one location.}
  \item{coord}{A matrix that gives the coordinates of each
    location. Each row corresponds to one location.}
   \item{marge}{Character string specifying how margins are transformed
    to unit Frechet. Must be one of "emp", "frech" or "mle" - see
    function \code{\link{fitextcoeff}}.}
  \item{iso}{Logical. If \code{TRUE}, isotropy is supposed. Otherwise
    (default), anisotropy is allowed.}
  \item{control}{The control arguments to be passed to the
    \code{\link{optim}} function.}
  \item{\dots}{Optional arguments to be passed to the \code{\link{optim}}
    function.}
  \item{start}{A named list giving the initial values for the
    parameters over which the weighted sum of square is to be
    minimized. If \code{start} is omitted the routine attempts to find
    good starting values.}
  \item{weighted}{Logical. Should weighted least squares be used?}
}
\details{
  The fitting procedure is based on weighted least squares. More
  precisely, the fitting criteria is to minimize:
  \deqn{\sum_{i,j} \left(\frac{\tilde{\theta}_{i,j} -
      \hat{\theta}_{i,j}}{s_{i,j}}\right)^2}{\sum_{i,j}
    [(\theta_{i,j}^+ - \theta_{i,j}^*) / s_{i,j}]^2}
  where \eqn{\tilde{\theta}_{i,j}}{\theta_{i,j}^+} is a non
  parametric estimate of the extremal coefficient related to location
  \code{i} and \code{j}, \eqn{\hat{\theta}_{i,j}}{\theta_{i,j}^*} is
  the fitted extremal coefficient derived from the Smith's model and
  \eqn{s_{i,j}}{s_{i,j}} are the standard errors related to the
  estimates \eqn{\tilde{\theta}_{i,j}}{\theta_{i,j}^+}.
}
\value{
  An object of class maxstab.
}
\references{
  Smith, R. L. (1990) Max-stable processes and spatial
  extremes. Unpublished manuscript.
  }
\author{Mathieu Ribatet}
\seealso{\code{\link{fitcovariance}}, \code{\link{fitmaxstab}},
  \code{\link{fitextcoeff}}}
\examples{
n.site <- 50
n.obs <- 100
locations <- matrix(runif(2*n.site, 0, 40), ncol = 2)
colnames(locations) <- c("lon", "lat")

## Simulate a max-stable process - with unit Frechet margins
data <- rmaxstab(50, locations, cov.mod = "gauss", cov11 = 200, cov12 =
0, cov22 = 200)

fitcovmat(data, locations)

##Force an isotropic model
fitcovmat(data, locations, iso = TRUE)
}
\keyword{htest}

