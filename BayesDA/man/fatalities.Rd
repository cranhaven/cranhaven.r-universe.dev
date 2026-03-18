\name{fatalities}
\alias{fatalities}
\docType{data}
\title{  Worldwide Airline Fatalities, 1976--1985   }
\description{
  Worldwide airline fatalities, 1976--1985.  Death rate is 
  passenger deaths per 100 million passenger miles. 
}
\usage{data(fatalities)}
\format{
  A data frame with 10 observations on the following 4 variables.
  \describe{
    \item{\code{year}}{year}
    \item{\code{facc}}{number of fatal accidents}
    \item{\code{pdeaths}}{number of passenger deaths}
    \item{\code{rdeath}}{death rate}
  }
}
\details{
  Source: Statistical Abstracts of the United States
}
\examples{
data(fatalities)
summary(fatalities)
}
\keyword{datasets}
