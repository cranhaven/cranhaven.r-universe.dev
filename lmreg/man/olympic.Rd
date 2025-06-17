\name{olympic}
\alias{olympic}
\docType{data}
\title{
Olympic sprint finals data
}
\description{
Times recorded by winners of men's olympic sprint finals in different categories from 1900 to 1988 (Lunn and McNeil, 1991).
}
\usage{data(olympic)}
\format{
  A data frame with 20 observations on the following 6 variables.
  \describe{
    \item{\code{Year}}{Olympic year}
    \item{\code{X100m}}{Winner's time (in seconds) for 100 meters sprint}
    \item{\code{X200m}}{Winner's time (in seconds) for 200 meters sprint}
    \item{\code{X400m}}{Winner's time (in seconds) for 400 meters sprint}
    \item{\code{X800m}}{Winner's time (in seconds) for 800 meters sprint}
    \item{\code{X1500m}}{Winner's time (in seconds) for 1500 meters sprint}
  }
}

\details{
There are three missing years in the data; 1916, 1940 and 1944, when world wars prevented the olympic games from being held.
}

\source{
Lunn, A.D. and McNeil, D.R. (1991) Computer-Interactive Data Analysis, Wiley, Chichester.
}

\examples{
data(olympic)
head(olympic) 
}
\keyword{datasets}
