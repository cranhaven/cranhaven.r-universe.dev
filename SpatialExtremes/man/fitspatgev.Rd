\name{fitspatgev}
\alias{fitspatgev}
\title{MLE for a spatial GEV model}
\description{
  This function derives the MLE of a spatial GEV model.
}
\usage{
fitspatgev(data, covariables, loc.form, scale.form, shape.form,
temp.cov = NULL, temp.form.loc = NULL, temp.form.scale = NULL,
temp.form.shape = NULL, ..., start, control = list(maxit = 10000),
method = "Nelder", warn = TRUE, corr = FALSE)
}
\arguments{
  \item{data}{A matrix representing the data. Each column corresponds to
    one location.}
  \item{covariables}{Matrix with named columns giving required
    covariates for the GEV parameter models.}
  \item{loc.form, scale.form, shape.form}{R formulas defining the
    spatial  models for the GEV parameters. See section Details.}
   \item{temp.cov}{Matrix with names columns giving additional *temporal*
    covariates for the GEV parameters. If \code{NULL}, no temporal trend
    are assume for the GEV parameters --- see section Details.}
  \item{temp.form.loc, temp.form.scale, temp.form.shape}{R formulas
    defining the temporal trends for the GEV parameters. May be
    missing. See section Details.}
  \item{start}{A named list giving the initial values for the
    parameters over which the pairwise likelihood is to be minimized. If
    \code{start} is omitted the routine attempts to find good starting
    values - but might fail.}
  \item{\dots}{Several arguments to be passed to the
    \code{\link{optim}} functions. See section details.}
  \item{control}{The control argument to be passed to the
    \code{\link{optim}} function.}
  \item{method}{The method used for the numerical optimisation
    procedure. Must be one of \code{BFGS}, \code{Nelder-Mead},
    \code{CG}, \code{L-BFGS-B} or \code{SANN}. See \code{\link{optim}}
    for details.}
  \item{warn}{Logical. If \code{TRUE} (default), users will be warned if
    the starting values lie in a zero density region.}
  \item{corr}{Logical. If \code{TRUE}, the asymptotic correlation matrix
    is computed.}
}
\details{
  A kind of "spatial" GEV model can be defined by using response
  surfaces for the GEV parameters. For instance, the GEV location
  parameters are defined through the following equation:

  \deqn{\mu = X_\mu \beta_\mu}{\mu = X_\mu \beta_\mu}
  where \eqn{X_\mu}{X_\mu} is the design matrix and
  \eqn{\beta_\mu}{\beta_\mu} is the vector parameter to be
  estimated. The GEV scale and shape parameters are defined accordingly
  to the above equation.

  The log-likelihood for the GEV spatial model is consequently defined
  as follows:

  \deqn{\ell(\beta) = \sum_{i=1}^{n.site} \sum_{j=1}^{n.obs} \log
    f(y_{i,j}; \theta_i)}{llik(\beta) = \sum_(i=1)^(n.site)
    \sum_(j=1)^(n.obs) log f(y_(i,j);\theta_i)}
  where \eqn{\theta_i}{\theta_i} is the vector of the GEV parameters for
  the \eqn{i}-th site.

  Most often, there will be some dependence between stations. However,
  it can be seen from the log-likelihood definition that we supposed
  that the stations are mutually independent. Consequently, to get
  reliable standard error estimates, these standard errors are estimated
  with their sandwich estimates.

  There are two different kind of covariates : "spatial" and
  "temporal".

  A "spatial" covariate may have different values accross station but
  does not depend on time. For example the coordinates of the stations
  are obviously "spatial". These "spatial" covariates should be used
  with the \code{marg.cov} and \code{loc.form, scale.form, shape.form}.

  A "temporal" covariates may have different values accross time but
  does not depend on space. For example the years where the annual
  maxima were recorded is "temporal". These "temporal" covariates should
  be used with the \code{temp.cov} and \code{temp.form.loc,
    temp.form.scale, temp.form.shape}.

  As a consequence note that \code{marg.cov} must have K rows (K being
  the number of sites) while \code{temp.cov} must have n rows (n being
  the number of observations).
}
\value{
  An object of class \code{spatgev}. Namely, this is a list with the
  following arguments:
  \item{fitted.values}{The parameter estimates.}
  \item{param}{All the parameters e.g. parameter estimates and fixed
    parameters.}
  \item{std.err}{The standard errors.}
  \item{var.cov}{The asymptotic MLE variance covariance matrix.}
  \item{counts,message,convergence}{Some information about the
    optimization procedure.}
  \item{logLik,deviance}{The log-likelihood and deviance values.}
  \item{loc.form, scale.form, shape.form}{The formulas defining the
    spatial models for the GEV parameters.}
  \item{covariables}{The covariables used for the spatial models.}
  \item{ihessian}{The inverse of the Hessian matrix of the negative
    log-likelihood.}
  \item{jacobian}{The variance covariance matrix of the score.}
}
\author{Mathieu Ribatet}
\examples{
## 1- Simulate a max-stable random field
n.site <- 35
locations <- matrix(runif(2*n.site, 0, 10), ncol = 2)
colnames(locations) <- c("lon", "lat")
data <- rmaxstab(50, locations, cov.mod = "whitmat", nugget = 0, range =
  3, smooth = 0.5)

## 2- Transformation to non unit Frechet margins
param.loc <- -10 + 2 * locations[,2]
param.scale <- 5 + 2 * locations[,1]
param.shape <- rep(0.2, n.site)
for (i in 1:n.site)
  data[,i] <- frech2gev(data[,i], param.loc[i], param.scale[i],
param.shape[i])

## 3- Fit a ''spatial GEV'' mdoel to data with the following models for
##    the GEV parameters
form.loc <- loc ~ lat
form.scale <- scale ~ lon
form.shape <- shape ~ 1

fitspatgev(data, locations, form.loc, form.scale, form.shape)
}
\keyword{htest}

