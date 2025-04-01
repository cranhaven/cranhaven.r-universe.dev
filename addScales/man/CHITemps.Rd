\name{CHITemps}
\alias{CHITemps}
\docType{data}
\title{
Daily Chicago High and Low Temperatures in \enc{°}{degrees}F
}
\description{
Chicago Midway Airport daily high and low temperatures from 1 March 1928 to 31 December 2019. There are some missing values, and the data are listed in chronological order.
}
\usage{CHITemps}
\format{
  A data frame with 32152 observations on the following 3 variables.
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
