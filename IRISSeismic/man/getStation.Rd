\name{getStation}
\docType{methods}
\alias{getStation}
\alias{getStation,IrisClient,character,character,character,character,POSIXct,POSIXct,logical-method}
\alias{getStation,IrisClient,character,character,character,character,POSIXct,POSIXct,missing-method}

\title{Retrieve Station metadata from IRIS DMC}

\description{
The \code{getStation} method obtains station metadata from the IRIS DMC station web service
and returns it in a dataframe.
}

\usage{
getStation(obj, network, station, location, channel,
           starttime, endtime, includerestricted,
           latitude, longitude, minradius, maxradius)
}

\arguments{
  \item{obj}{ \code{IrisClient} object }
  \item{network}{ character string with the two letter seismic network code }
  \item{station}{ character string with the station code }
  \item{location}{ character string with the location code }
  \item{channel}{ character string with the three letter channel code }
  \item{starttime}{ POSIXct class specifying the starttime (GMT) }
  \item{endtime}{ POSIXct class specifying the endtime (GMT) }
  \item{includerestricted}{ optional logical identifying whether to report on restricted data }
  \item{latitude}{ optional latitude used when specifying a location and radius }
  \item{longitude}{ optional longitude used when specifying a location and radius }
  \item{minradius}{ optional minimum radius used when specifying a location and radius }
  \item{maxradius}{ optional maximum radius used when specifying a location and radius }
}


\details{
The \code{getStation} method utilizes the station web service to obtain data for all stations that meet the criteria defined by the arguments
and returns that data in a dataframe. Each row of the dataframe represents a unique station.

Each of the arguments \code{network}, \code{station}, \code{location} or \code{channel} may contain
a valid code or a wildcard expression, e.g. "BH?" or "*". Empty strings are converted to "*".
Otherwise, the ascii string that is used for
these values is simply inserted into the web service request URL.

For more details see the \href{https://service.iris.edu/fdsnws/station/1/}{web service documentation}.
}

\value{
A dataframe with the following columns:

%% max preformatted length ---------------------------------------------------
\preformatted{
network, station, latitude, longitude, elevation, sitename, starttime, endtime
}

Rows are ordered by \code{network-station}.
}

\references{
%% ~put references to the literature/web site here ~
The IRIS DMC station web service:

  \url{https://service.iris.edu/fdsnws/station/1/}

This implementation was inspired by the functionality in the obspy get_stations() method.
 
  \url{https://docs.obspy.org/packages/autogen/obspy.clients.fdsn.client.Client.get_stations.html}

}

\author{
Jonathan Callahan \email{jonathan@mazamascience.com}
}

%% \note{ }

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{IrisClient-class}}
}

\examples{
# Open a connection to IRIS DMC webservices
iris <- new("IrisClient")

# Date of Nisqually quake
starttime <- as.POSIXct("2001-02-28",tz="GMT")
endtime <- starttime + 2*24*3600

# Use the getEvent web service to determine what events happened in this time period
result <- try(events <- getEvent(iris,starttime,endtime,6.0))
if (inherits(result,"try-error")) {
  message(geterrmessage())
} else {
  events

# biggest event is Nisqually
  eIndex <- which(events$magnitude == max(events$magnitude))
  e <- events[eIndex[1],]

# Which stations in the US network are within 5 degrees of the quake epicenter?
  result <- try(stations <- getStation(iris,"US","*","*","BHZ",starttime,endtime,
                            lat=e$latitude,long=e$longitude,maxradius=5))
  if (inherits(result,"try-error")) {
    message(geterrmessage())
  } else {
    stations
  }
}
}

\keyword{webservices}
