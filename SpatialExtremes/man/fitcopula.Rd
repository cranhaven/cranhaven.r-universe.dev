\name{fitcopula}
\alias{fitcopula}
\title{
  Fit a copula-based model to spatial extremes
}
\description{
  This function fits various copula-based models to spatial extremes
  data sets.
}
\usage{
fitcopula(data, coord, copula = "gaussian", cov.mod = "whitmat",
loc.form, scale.form, shape.form, marg.cov = NULL, temp.cov = NULL,
temp.form.loc = NULL, temp.form.scale = NULL, temp.form.shape = NULL,
..., start, control = list(maxit = 10000), method = "Nelder", std.err =
TRUE, warn = TRUE, corr = FALSE)
}
\arguments{
  \item{data}{A matrix representing the data. Each column corresponds to
    one location.}
  \item{coord}{A matrix that gives the coordinates of each
    location. Each row corresponds to one location.}
  \item{copula}{A character string. Must be one of "gaussian" and
    "student" for a Gaussian and Student copula.}
  \item{cov.mod}{A character string corresponding to the correlation
    function family used in the copula. Must be one of "whitmat",
    "cauchy", "powexp", "bessel" or "caugen" for the Whittle-Matern, the
    Cauchy, the Powered Exponential, the Bessel and the Generalized
    Cauchy correlation families.}
  \item{loc.form, scale.form, shape.form}{R formulas defining the
    spatial linear model for the GEV parameters. May be missing. See
    section Details of function \code{\link{fitmaxstab}}.}
  \item{marg.cov}{Matrix with named columns giving additional covariates
    for the GEV parameters. If \code{NULL}, no extra covariates are
    used.}
  \item{temp.cov}{Matrix with names columns giving additional *temporal*
    covariates for the GEV parameters. If \code{NULL}, no temporal trend
    are assume for the GEV parameters --- see section Details of
    function \code{\link{fitmaxstab}}.}
  \item{temp.form.loc, temp.form.scale, temp.form.shape}{R formulas
    defining the temporal trends for the GEV parameters. May be
    missing. See section Details of function \code{\link{fitmaxstab}}.}
  \item{\dots}{Several arguments to be passed to the
    \code{\link{optim}}, \code{\link{nlm}} or \code{\link{nlminb}}
    functions.}
  \item{start}{A named list giving the initial values for the
    parameters over which the pairwise likelihood is to be minimized. If
    \code{start} is omitted the routine attempts to find good starting
    values - but might fail.}
  \item{control}{A list giving the control parameters to be passed to
    the \code{\link{optim}} function.}
  \item{method}{The method used for the numerical optimisation
    procedure. Must be one of \code{BFGS}, \code{Nelder-Mead},
    \code{CG}, \code{L-BFGS-B}, \code{SANN}, \code{nlm} or
    \code{nlminb}. See \code{\link{optim}} for details. Please note that
    passing \code{nlm} or \code{nlminb} will use the \code{\link{nlm}}
    or \code{\link{nlminb}} functions instead of \code{\link{optim}}.}
  \item{std.err}{Logical. Should the standard errors be computed ? The
    default is to return the standard errors, i.e.,  \code{std.err =
      TRUE}.}
  \item{warn}{Logical. If \code{TRUE} (default), users are warned if
    the log-likelihood is infinite at starting values and/or problems
    arised while computing the standard errors.}
  \item{corr}{Logical. If \code{TRUE} (non default), the asymptotic
    correlation matrix is computed.}
}
\value{
  This function returns a object of class \code{copula}.
}
\section{Warning}{
  This function does not use max-stable copula and the use of non
  max-stable copula for modelling spatial extreme is highly
  questionable. This function was mainly implemented for educational
  purposes and \bold{not} for concrete modelling purposes. 
}
\references{
  Davison, A.C., Padoan, S.A., Ribatet, M. (2010) Statistical Modelling
  of Spatial Extremes. \emph{Submitted to Statistical Science}.
}
\author{
  Mathieu Ribatet
}
\seealso{
  \code{\link{fitmaxstab}}, \code{\link{latent}}
}
\examples{
\dontrun{
n.site <- 30
n.obs <- 50

coord <- matrix(runif(2 * n.site, -10, 10), ncol = 2)
colnames(coord) <- c("lon", "lat")

## Generate data from a Gaussian copula model
data <- rcopula(n.obs, coord, "gaussian", "powexp", nugget = 0, range = 4, smooth = 1.2)

## Transform the margins to GEV
locs <- -5 + coord[,"lon"] / 10
scales <- 10 + coord[,"lat"] / 2
shapes <- rep(0.2, n.site)

for (i in 1:n.site)
  data[,i] <- frech2gev(data[,i], locs[i], scales[i], shapes[i])

## Fit a Gaussian copula model

## 1. Define trend surfaces
loc.form <- y ~ lon
scale.form <- y ~ lat
shape.form <- y ~ 1

## 2. Fit
M0 <- fitcopula(data, coord, "gaussian", "powexp", loc.form, scale.form,
                shape.form, nugget = 0)
}
}
\keyword{htest}
