\name{Airlines}
\alias{Airlines}
\docType{data}
\title{Ontime Records for Two Airlines at Two Airports}
\description{
Ontime arrivals for American and Delta airlines at LaGuardia and O'Hare airports
}
\format{
  A data frame with 10333 observations on the following 5 variables.
  \describe{
    \item{\code{airline}}{\code{American} or \code{Delta}}
    \item{\code{airport}}{ \code{LGA}=LaGuardia \code{ORD}=O'Hare}
    \item{\code{OnTime}}{ \code{no} or \code{yes}}
    \item{\code{IndOHare}}{Is the airport ORD? (\code{1}=yes or \code{0}=no) }
    \item{\code{IndDelta}}{Is the airline Delta? (\code{1}=yes or \code{0}=no)}
    }
}
\details{
Ontime/late data for individual flights to LaGuardia and O'Hare airports by American and Delta airlines.
}
\source{
Data collected on 9/20/16 from
http://www.transtats.bts.gov/ot_delay/OT_DelayCause1.asp?pn=1
}
\keyword{datasets}
