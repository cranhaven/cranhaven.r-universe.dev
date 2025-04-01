\name{SFTemps}
\alias{SFTemps}
\docType{data}
\title{
Daily San Francisco High and Low Temperatures in \enc{°}{degrees}F
}
\description{
San Francisco daily high and low temperatures from 1 March 1949 to 28 February 2020. There are some missing values, and the data are listed in chronological order.
}
\usage{SFTemps}
\format{
  A data frame with 22406 observations on the following 3 variables.
  \describe{
    \item{\code{DATE}}{character vector of form "YYYY-MM-DD"}
    \item{\code{TMAX}}{numeric, daily high temperature}
    \item{\code{TMIN}}{numeric, daily low temperature}
  }
}

\source{
NOAA National Centers for Environmental Information:
Climate Data Online

\url{https://www.ncdc.noaa.gov/cdo-web/}
}

\keyword{datasets}
