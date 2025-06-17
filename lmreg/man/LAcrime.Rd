\name{LAcrime}
\alias{LAcrime}
\docType{data}
\title{
LA crime and temperature data
}
\description{
Monthly total counts of homicides and rapes in the city of Los Angeles from January 1975 to December 1993.
}
\usage{data(LAcrime)}
\format{
  A data frame with 228 observations on the following 7 variables.
  \describe{
    \item{\code{Year}}{Year of record}
    \item{\code{Month}}{Month of record}
    \item{\code{Population}}{Population of the city in the year of record}
    \item{\code{TempCelsius}}{Monthly average temperature recorded at the Los Angeles International Airport (in Celsius)}
    \item{\code{Fahrenheit}}{Monthly average temperature recorded at the Los Angeles International Airport (in Fahrenheit)}
    \item{\code{Homicide}}{Total count of homicides in the month and year of record}
    \item{\code{Rape}}{Total count of rapes in the month and year of record}
  }
}

\source{
The crime data: Carlson, S.M. (1998), Uniform Crime Reports: Monthly Weapon-Specific Crime and Arrest Time Series, 1975-1993, ICPSR06792-v1, Interuniversity Consortium for Political and Social Research, Ann Arbor, MI (https://www.icpsr.umich.edu/icpsrweb/NACJD/studies/6792). 
Temperature data for LAX (WMO ID 72295): National Oceanic and Atmospheric Administration, USA (http://www.ncdc.noaa.gov/ghcnm/v2.php)
}

\examples{
data(LAcrime)
head(LAcrime)
}
\keyword{datasets}
