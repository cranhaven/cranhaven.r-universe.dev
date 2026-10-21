\name{lsmaxstab}
\alias{lsmaxstab}
\title{Estimates the spatial dependence parameter of a max-stable
  process by minimizing least squares.}
\description{
  Estimates the spatial dependence parameter of a max-stable
  process by minimizing least squares.
}
\usage{
lsmaxstab(data, coord, cov.mod = "gauss", marge = "emp", control =
list(), iso = FALSE, \dots, weighted = TRUE)
}
\arguments{
  \item{data}{A matrix representing the data. Each column corresponds to
    one location.}
  \item{coord}{A matrix that gives the coordinates of each
    location. Each row corresponds to one location.}
  \item{cov.mod}{Character string specifying the max-stable process
    considered. Must be one of "gauss" (Smith's model), "whitmat",
    "cauchy", "powexp", "bessel", "caugen" for the Schlather model with
    the corresponding correlation function.}
   \item{marge}{Character string specifying how margins are transformed
    to unit Frechet. Must be one of "emp", "frech" or "mle" - see
    function \code{\link{fitextcoeff}}.}
  \item{control}{The control arguments to be passed to the
    \code{\link{optim}} function.}
  \item{iso}{Logical. If \code{TRUE}, isotropy is supposed. Otherwise
    (default), anisotropy is allowed. Currently this is only useful for
    the Smith model.}
  \item{\dots}{Optional arguments.}
  \item{weighted}{Logical. Should weighted least squares be used? See
    Details.}
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
  the fitted extremal coefficient derived from the maxstable model
  considered and \eqn{s_{i,j}}{s_{i,j}} are the standard errors related
  to the estimates \eqn{\tilde{\theta}_{i,j}}{\theta_{i,j}^+}.
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

lsmaxstab(data, locations, "gauss")

##Force an isotropic model and do not use weights
lsmaxstab(data, locations, "gauss", iso = TRUE, weighted = FALSE)
}
\keyword{htest}

