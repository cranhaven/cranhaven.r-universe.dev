\name{pRsim.weather}
\alias{PRsim.weather}
\alias{prsim.weather}
\alias{prsim_weather}
\title{Weather simulation (temperature and precipitation) for multiple stations}
\description{
Applies the wavelet-based weather simulation algorithm to multiple sites (single site possible as well)
}
\usage{
prsim.weather(data_p, data_t, station_id_p="Precip",
station_id_t="Temp", number_sim=1, win_h_length=15,
n_wave=100,verbose=TRUE,t_margin='sep',p_margin='egpd',...)
}
\arguments{
  \item{data_p}{list of precipitation data frames. One list entry, i.e. data frame, corresponds to one station/grid cell. Each data frame contains the time indications and precipitation of one station. See \sQuote{Details}.}
    \item{data_t}{list of temperature data frames. One list entry, i.e. data frame, corresponds to one station/grid cell. Each data frame contains the time indications and temperature of one station. See \sQuote{Details}.}
  \item{station_id_p}{identifies the precipitation variable name in case several time series are present in \code{data_p}. See \sQuote{Details}.}
    \item{station_id_t}{identifies the temperature variable name in case several time series are present in \code{data_t}. See \sQuote{Details}.}
  \item{number_sim}{number of simulations to be carried out.}
  \item{win_h_length}{(half-)length of moving window size.}
  \item{t_margin}{marginal distribution to be used for the backtransformation of temperature. Can be either \code{"sep"} or any type of CDF (see \sQuote{Details}). \code{"sep"} uses the four-parameter skewed-exponential power for backtransformation. CDF allows for specifying any distribution \sQuote{Examples}.}
  \item{p_margin}{marginal distribution to be used for the backtransformation of precipitation. Can be either \code{"egpd"} or any type of CDF (see \sQuote{Details}). \code{"egpd"} uses the extended GPD for backtransformation. CDF allows for specifying any distribution \sQuote{Examples}.}
  \item{verbose}{logical. Should progress be reported?}
  \item{n_wave}{number of scales to be considered in the continuous wavelet transform.}
  \item{...}{any other argument passed to the sub-function specifying the cdf for fitting.  See \sQuote{Details} and  \sQuote{Examples}. }
}
\details{
Time can be given with three columns named \code{"YYYY"}, \code{"MM"}, \code{"DD"}, or as in POSIXct format \code{YYYY-MM-DD}.
All leap days (Feb 29th) will be omitted from the analysis, but no missing observations are allowed.

Stations are identified by list index.

Alternative distributions can be specified by providing three functions: (1) a function fitting the parameters of a distributions and providing a vector of these parameters as output (CDF_fit), (2) a function simulating random numbers from this distribution (rCDF), and (3) a function specifying the distribution (pCDF). See \sQuote{Examples} for the generalized beta for the second kind and for the Generalized Extreme Values (GEV) distribution.

}
\value{A list with elements temperature and precipitation of
 \item{simulation}{data frames with time information, observations, and 
       \code{number_sim} columns containing the simulated data.}
}
\references{
Brunner, M. I., and E. Gilleland (2021). Spatial compound hot-dry events in the United States: assessment using a multi-site multi-variable weather generator, in preparation.}
\author{
Manuela Brunner
}

\keyword{ts}
