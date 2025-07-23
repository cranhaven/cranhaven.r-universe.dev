

\name{get_DayTimeLimits}
\alias{get_DayTimeLimits}
\title{
Estimating the timing of sunrise, sunset, astronomical and nautical twilight events
}

\description{
Estimating the timing of sunrise, sunset, astronomical and nautical twilight events in \code{POSIXct}-format based on geolocations and a similar time vector. The function is a simplified call of the \link{sunriset} and \link{crepuscule} functions of the maptools-package that are based on algorithms provided by the National Oceanic & Atmospheric Administration (NOAA). 
}

\usage{
get_DayTimeLimits(pos)
}
 
\arguments{
\item{pos}{a \link{data.frame} with the columns \code{datetime} (a time vector in \code{POSIXct}-format), \code{Lon} and \code{Lat}.
}
}

\value{
The input \link{data.frame} \code{pos} extended by the time vectors \code{sunrise}, \code{sunset}, \code{dawn.naut}, \code{dawn.ast}, \code{dusk.naut} and \code{dusk.ast}.
}




\references{
Meeus, J. (1991) Astronomical Algorithms. Willmann-Bell, Inc.
}


\seealso{\link{sunriset}, \link{crepuscule}, \link{classify_DayTime}
}


\author{
Robert K. Bauer
}

\examples{

#### example 1) estimate current times of dawn, sunrise, dusk and sunset in Mainz, Germany:
pos <- data.frame(Lat=8.2667, Lon=50)
pos$datetime <- strptime(Sys.Date(),"\%Y-\%m-\%d")
get_DayTimeLimits(pos)

#### example 1b) classify current ime of the day in Mainz, Germany:
classify_DayTime(get_DayTimeLimits(pos))


}

