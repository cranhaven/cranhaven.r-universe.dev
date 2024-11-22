\name{getAvailability}
\docType{methods}
\alias{getAvailability}
\alias{getAvailability,IrisClient,character,character,character,character,POSIXct,POSIXct,logical-method}
\alias{getAvailability,IrisClient,character,character,character,character,POSIXct,POSIXct,missing-method}

\title{Retrieve Channel metadata from IRIS DMC}

\description{
The \code{getAvailability} method obtains channel metadata for available channels from the IRIS DMC
\code{station} web service and returns it in a dataframe.
}

\usage{
getAvailability(obj, network, station, location, channel,
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
  \item{includerestricted}{ optional logical identifying whether to report on restricted data (default=\code{FALSE}) }
  \item{latitude}{ optional latitude used when specifying a location and radius }
  \item{longitude}{ optional longitude used when specifying a location and radius }
  \item{minradius}{ optional minimum radius used when specifying a location and radius }
  \item{maxradius}{ optional maximum radius used when specifying a location and radius }
}


\details{
The \code{getAvailability} method uses the station web service to obtain data for all
available channels that meet the criteria defined by the arguments
and returns that data in a dataframe. Each row of the dataframe represents a unique channel-epoch. 
This method is equivalent to the \code{getChannel}
method with the following additional parameters attached to the url:

\preformatted{&includeavailability=true&matchtimeseries=true}

Each of the arguments \code{network}, \code{station}, \code{location} or \code{channel} may contain
a valid code or a wildcard expression, e.g. "BH?" or "*". Empty strings are converted to "*".
Otherwise the ascii string that is used for
these values is simply inserted into the web service request URL.
(For non-available channels use \code{\link{getUnavailability}}.)

For more details see the \href{https://service.iris.edu/fdsnws/station/1/}{web service documentation}.
}

\value{
A dataframe with the following columns:

%% max preformatted length ---------------------------------------------------
\preformatted{
network, station, location, channel, latitude, longitude, elevation,
  depth, azimuth, dip, instrument, scale, scalefreq, scaleunits, 
  samplerate, starttime, endtime, snclId
}

Rows are ordered by \code{snclId}.

The snclId column, eg. "US.OCWA..BHE", is generated as a convenience. It is not part of
the normal return from the station web service.

\bold{Note:}  The \code{snclId} is not a unique identifier. If the time span of interest
crosses an epoch boundary where instrumentation was changed then multiple records (rows)
will share the same \code{snclId}.
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
\code{\link{IrisClient-class}}, \code{\link{getChannel}}, \code{\link{getUnavailability}}
}

%% max preformatted length ---------------------------------------------------
\examples{
# Open a connection to IRIS DMC webservices
iris <- new("IrisClient")

# Date of Nisqually quake
starttime <- as.POSIXct("2001-02-28",tz="GMT")
endtime <- starttime + 2*24*3600

# Use  getEvent web service to retrieve events in this time period
result <- try(events <- getEvent(iris,starttime,endtime,6.0))
if (inherits(result,"try-error")) {
  message(geterrmessage())
} else {
  events

# biggest event is Nisqually
  eIndex <- which(events$magnitude == max(events$magnitude))
  e <- events[eIndex[1],]

# Find all BHZ channels collecting data at the time of the quake and within 
# 5 degrees of the quake epicenter
  result <- try(channels <- getAvailability(iris,"*","*","*","BHZ",starttime,endtime,
                            lat=e$latitude,long=e$longitude,maxradius=5))
  if (inherits(result,"try-error")) {
    message(geterrmessage())
  } else {
    channels
  }
}
}

\keyword{webservices}
