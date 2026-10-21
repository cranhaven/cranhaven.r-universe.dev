\name{fitextcoeff}
\alias{fitextcoeff}
\title{Non parametric estimators of the extremal coefficient function}
\description{Estimates non parametrically the extremal coefficient
  function.
}
\usage{
fitextcoeff(data, coord, \dots, estim = "ST", marge = "emp", prob = 0,
plot = TRUE, loess = TRUE, method = "BFGS", std.err = TRUE, xlab,
ylab, angles = NULL, identify = FALSE)
}
\arguments{
  \item{data}{A matrix representing the data. Each column corresponds to
    one location.}
  \item{coord}{A matrix that gives the coordinates of each
    location. Each row corresponds to one location.}
  \item{\dots}{Additional options to be passed to the \code{\link{plot}}
    function.}
  \item{estim}{Character string specifying the estimator to be
    used. Must be one of "ST" (Schlather and Tawn) or "Smith".}
  \item{marge}{Character string specifying how margins are transformed
    to unit Frechet. Must be one of "emp", "mle" or "none" - see
    Details}
  \item{prob}{The probability related to the threshold. Only useful with
    the \code{ST} estimator.}
  \item{plot}{Logical. If \code{TRUE} (default), the extremal
    coefficient function is plotted}
  \item{loess}{If \code{TRUE} (default), a local polynomial curve is
    plotted - see function \code{\link{loess}}.}
  \item{method}{The optimizer used when fitting the GEV distribution to
    data. See function \code{\link{gevmle}}.}
  \item{std.err}{Logical. If \code{TRUE}, standard errors are
    computed. Note that standard errors are not available with the "ST"
    estimator.}
  \item{xlab,ylab}{The x-axis and y-axis labels. May be missing.}
  \item{angles}{A numeric vector. A partition of the interval
    \eqn{(-\pi, \pi)}{(-\pi, \pi)} to help detecting anisotropy.}
  \item{identify}{Logical. If \code{TRUE}, users can use the
    \code{\link{identify}} function to identify pairs of stations on the
    plot.}
}
\details{
  During the estimation procedure, data need to be transformed to unit
  Frechet margins firts. This can be done in two different ways ; by
  using the empirical CDF or the GEV ML estimates.

  If \code{marge = "emp"}, then the data are transformed using the
  following relation:  
  \deqn{z_i = - \frac{1}{\log (F(y_i))}}{z_i = - 1 / log(F(y_i))}
  where \eqn{y_i}{y_i} are the observations available at location
  \eqn{i}{i}, \eqn{F}{F} is the empirical CDF and \eqn{z_i}{z_i} are the
  observations transformed to unit Frechet scale.

  If \code{marge = "mle"}, then the data are transformed using the MLE
  of the GEV distribution - see function \code{\link{gev2frech}}.

  Lastly, if data are already supposed to be unit Frechet, then no
  transformation is performed if one passed the option \code{marge =
  "frech"}.
  
  If \code{data} are already componentwise maxima, \code{prob} should be
  zero. Otherwise, users have to define a threshold \eqn{z}{z} (large
  enough) where univariate extreme value arguments are relevant. We
  define \code{prob} such that \eqn{\Pr[Z \leq z] = prob}{Pr[Z <= z] =
    prob}.
}
\value{
  Plots the extremal coefficient function and returns the points used
  for the plot. If \code{loess = TRUE}, the output is a list with
  argument "ext.coeff" and "loess".
}
\references{
  Schlather, M. and Tawn, J. A. (2003) A dependence measure for
  multivariate and spatial extreme values: Properties and
  inference. Biometrika \bold{90}(1):139--156.

  Smith, R. L. (1990) Max-stable processes and spatial
  extremes. Unpublished manuscript.
}
\seealso{
  \code{\link{madogram}}
}
\examples{
n.site <- 30
locations <- matrix(runif(2*n.site, 0, 10), ncol = 2)
colnames(locations) <- c("lon", "lat")

##Simulate a max-stable process - with unit Frechet margins
data <- rmaxstab(50, locations, cov.mod = "gauss", cov11 = 10, cov12 =
40, cov22 = 220)

##Plot the extremal coefficient function
op <- par(mfrow=c(1,2))
fitextcoeff(data, locations, estim = "Smith")
fitextcoeff(data, locations, angles = seq(-pi, pi, length = 4), estim = "Smith")
par(op)
}
\author{Mathieu Ribatet}
\keyword{hplot}
