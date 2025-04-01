\name{USAcrime}
\alias{USAcrime}
\docType{data}
\title{
USA Property and Violent Crime Data, 1960 - 2014
}
\description{
USA crime rates as incidents/100,000 population by state for several categories of property and violent crimes from 1960 - 2014 (except for New York, which starts in 1965).
}
\usage{data("USAcrime")}
\format{
  A data frame with 2745 observations on the following 12 variables.
  \describe{
    \item{\code{Year}}{numeric}
    \item{\code{State}}{a factor with \code{state.abb} as its levels}
    \item{\code{Population}}{estimated population, numeric}
    \item{\code{allViolent}}{overall estimated violent crime rate, numeric}
    \item{\code{Homicide}}{numeric}
    \item{\code{Rape}}{numeric}
    \item{\code{Robbery}}{numeric}
    \item{\code{Assault}}{numeric}
    \item{\code{allProperty}}{overall estimated property crime rate, numeric}
    \item{\code{Burglary}}{numeric}
    \item{\code{Larceny}}{numeric}
    \item{\code{vehicleTheft}}{numeric}
   }
}
\details{
Note that these are estimates, and there are various caveats and inconsistencies in definitions and reporting methods among states and over time. Consult the source for details.
}
\note{
The data are ordered by state and year within state. The levels of the (unordered) State factor are \code{state.abb} but in the full state name order of \code{state.name}. Hence, for example, AL will procede AK and AZ will precede AR in the default ordering of the levels, because Alabama precedes Alaska and Arizona precedes Arkansas in lexicographic order.
}

\source{
FBI Uniform Crime Reporting Statistics
\url{https://www.ucrdatatool.gov/Search/Crime/State/StatebyState.cfm}
}

\keyword{datasets}
