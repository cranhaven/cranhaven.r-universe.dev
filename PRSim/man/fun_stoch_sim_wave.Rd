\name{pRsim.wave}
\alias{PRsim.wave}
\alias{prsim.wave}
\alias{prsim_wave}
\title{Simulate for multiple stations}
\description{
Applies the wavelet-based simulation algorithm to multiple sites (single site possible as well)
}
\usage{
prsim.wave(data, station_id="Qobs", number_sim=1, win_h_length=15, 
                  marginal=c("kappa","empirical"), n_par=4, n_wave=100, marginalpar=TRUE, 
                  GoFtest=NULL, verbose=TRUE, suppWarn=FALSE, ...)
}
\arguments{
  \item{data}{list of data frames. One list entry, i.e. data frame, corresponds to one station. Each data frame contains the time indications and runoff of one station. See \sQuote{Details}.}
  \item{station_id}{identifies the station in case several time series are present in \code{data}. See \sQuote{Details}.}
  \item{number_sim}{number of simulations to be carried out.}
  \item{win_h_length}{(half-)length of moving window size.}
  \item{marginal}{marginal distribution to be used for the backtransformation. Can be either \code{"kappa"}, \code{"empirical"}, or any type of CDF (see \sQuote{Details}). \code{"kappa"} uses the four-parameter kappa distribution for backtransformation, \code{"empirical"} uses the empirical distribution. CDF allows for specifying any distribution \sQuote{Examples}.}
  \item{n_par}{number of parameters of the marginal distribution used}
  \item{GoFtest}{If (non-null) a GoF test for daily data should be performed: \code{"KS"} performs a Kolmogorof-Smirnov test, and \code{"AD"} performs an Anderson-Darling test.  see \sQuote{Details})}
  \item{verbose}{logical. Should progress be reported?}
  \item{marginalpar}{logical. Should the estimated parameters of the distribution used be returned?}
  \item{n_wave}{number of scales to be considered in the continuous wavelet transform.}
  \item{suppWarn}{logical. See \sQuote{Details}.}
  \item{...}{any other argument passed to the sub-function specifying the cdf for fitting.  See \sQuote{Details} and  \sQuote{Examples}. }
}
\details{
Time can be given with three columns named \code{"YYYY"}, \code{"MM"}, \code{"DD"}, or as in POSIXct format \code{YYYY-MM-DD}.
All leap days (Feb 29th) will be omitted from the analysis, but no missing observations are allowed.

Stations are identified by list index.


The function \code{homtest::par.kappa} might issue quite a few warnings of type \code{In fn(par, ...) : value out of range in 'gammafn'}. The argument \code{suppWarn} allows to silence warnings for the specific function call via \code{suppressWarnings()}. Of course, a subsequent check via \code{warnings()} is recommended.

Alternative distributions can be specified by providing three functions: (1) a function fitting the parameters of a distributions and providing a vector of these parameters as output (CDF_fit), (2) a function simulating random numbers from this distribution (rCDF), and (3) a function specifying the distribution (pCDF). See \sQuote{Examples} for the generalized beta for the second kind and for the Generalized Extreme Values (GEV) distribution.

When using the kappa distribution, the AD test can for certain values of the parameter h not be performed. 

}
\value{A list with elements 
 \item{simulation}{A data frame with time information, observations, and 
       \code{number_sim} columns containing the simulated runoff.}
  \item{pars}{A matrix containing the estimated parameters of the marginal distribution (if \code{marginalpar}).}
 \item{p_val}{A vector containing the p-values of \code{ks.test} or \code{ad.test} applied to the daily detrended data (if \code{GoFtest} is not NULL)}
}
\references{
Brunner, M. I., and E. Gilleland (2020). Stochastic simulation of streamflow and spatial extremes: a continuous, wavelet-based approach, Hydrology and Earth System Sciences, https://doi.org/10.5194/hess-24-3967-2020.}
\author{
Manuela Brunner
}

\seealso{
\code{ks.test}
}
\examples{
data(runoff_multi_sites)

## Specifying particular CDFs:
## (1) example with the Generalized Extreme Value (GEV) distribution
\donttest{
require("evd")
require("ismev")
rGEV <- function(n, theta)   rgev(n, theta[1], theta[2], theta[3])
pGEV <- function(x, theta)   pgev(x, theta[1], theta[2], theta[3])
GEV_fit <- function( xdat, ...)   gev.fit( xdat, ...)$mle
}

## (2) example with generalized Beta distribution of the second kind
\donttest{
require( "GB2")
rGB2 <- function(n, theta)  rgb2(n, theta[1], theta[2], theta[3], theta[4])
pGB2 <- function(x, theta)  pgb2(x, theta[1], theta[2], theta[3], theta[4])
GB2_fit <- function( xdat, ...)   ml.gb2( xdat, ...)$opt1$par
}

}
\keyword{ts}
