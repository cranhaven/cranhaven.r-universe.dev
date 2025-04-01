\name{NYCTemps}
\alias{NYCTemps}
\docType{data}
\title{
Daily New York City High and Low Temperatures in \enc{°}{degrees}F
}
\description{
New York City Central Park daily high and low temperatures from 1 January 1870 to 12 December 2019. There are no missing values, and the data are listed in chronological order.
}
\usage{NYCTemps}
\format{
  A data frame with 54786 observations on the following 3 variables.
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
