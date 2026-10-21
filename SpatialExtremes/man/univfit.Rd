\name{margin fits}
\alias{gevmle}
\alias{gpdmle}
\title{Fits univariate extreme value distributions to data}
\description{These functions fit the generalised extreme value and
  generalised Pareto distribution to data using maximum likelihood.
}
\usage{
gevmle(x, \dots, method = "Nelder")
gpdmle(x, threshold, \dots, method = "Nelder")
}
\arguments{
  \item{x}{Numeric vector of observations}
  \item{\dots}{Optional arguments to be passed to the
    \code{\link{optim}} function.}
  \item{threshold}{Numeric. The threshold value.}
  \item{method}{The numerical optimisation method to be used.}  
}
\value{
  A vector for the estimated parameters of the GEV/GPD.
}
\details{
  These two functions are ``extremely light'' functions to fit the
  GEV/GPD. These functions are mainly useful to compute starting values
  for the Schlather and Smith model - see \code{\link{fitmaxstab}}.

  If more refined (univariate) analysis have to be performed, users should
  use more specialised packages - e.g. POT, evd, ismev, \dots.
}
\examples{
## 1 - GEV fit
x <- rep(NA, 100)
for (i in 1:100)
  x[i] <- max(rnorm(365))

gevmle(x)

## 2- GPD fit
x <- rnorm(10000)
##we need to fix a threshold
u <- quantile(x, 0.99)
gpdmle(x, u)
}
\author{Mathieu Ribatet}
\keyword{htest}
