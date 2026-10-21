\name{fitcovariance}
\alias{fitcovariance}
\title{Estimates the covariance function for the Schlather's model}
\description{
  Estimates the covariance function for the Schlather's model using
  non-parametric estimates of the pairwise extremal coefficients.
}
\usage{
fitcovariance(data, coord, cov.mod, marge = "emp", control = list(),
..., start, weighted = TRUE)
}
\arguments{
  \item{data}{A matrix representing the data. Each column corresponds to
    one location.}
  \item{coord}{A matrix that gives the coordinates of each
    location. Each row corresponds to one location.}
  \item{cov.mod}{A character string corresponding the the covariance
    model in the Schlather's model. Must be one of "whitmat", "cauchy",
    "powexp", "bessel" or "caugen" for the Whittle-Matern, the Cauchy,
    the Powered Exponential, the Bessel and the Generalized Cauchy
    correlation families.}
  \item{marge}{Character string specifying how margins are transformed
    to unit Frechet. Must be one of "emp", "frech" or "mle" - see
    function \code{\link{fitextcoeff}}.}
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
  the fitted extremal coefficient derived from the Schlather's model and
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
\seealso{\code{\link{fitcovmat}}, \code{\link{fitmaxstab}},
  \code{\link{fitextcoeff}}}
\examples{
n.site <- 50
locations <- matrix(runif(2*n.site, 0, 10), ncol = 2)
colnames(locations) <- c("lon", "lat")

##Simulating a max-stable process using RandomFields
##This is the Schlather's approach
data <- rmaxstab(50, locations, cov.mod = "whitmat", nugget = 0, range =
30, smooth = 1)

fitcovariance(data, locations, "whitmat")
}
\keyword{htest}

