\name{getEvent}
\docType{methods}
\alias{getEvent}
\alias{getEvent,IrisClient,POSIXct,POSIXct-method}

\title{Retrieve seismic event information from the USGS NEIC}

\description{
The \code{getEvent} method obtains seismic event data from
the USGS NEIC \code{event} webservice.
}

\usage{
getEvent(obj, starttime, endtime, minmag, maxmag, magtype,
         mindepth, maxdepth)
}

\arguments{
  \item{obj}{ an \code{IrisClient} object }
  \item{starttime}{ POSIXct class limiting results to events occurring after starttime (GMT) }
  \item{endtime}{ POSIXct class limiting results to events occurring before endtime (GMT) }
  \item{minmag}{ optional minimum magnitude }
  \item{maxmag}{ optional maximum magnitude }
  \item{magtype}{ optional magnitude type }
  \item{mindepth}{ optional minimum depth (km) }
  \item{maxdepth}{ optional maximum depth (km) }
}

\details{
The \code{getEvent} method uses the event web service to obtain data for all events that meet the criteria defined by the arguments
and returns that data in a dataframe. Each row of the dataframe represents a unique event.

\code{getEvent} calls to the IRIS event webservice now go to https://earthquake.usgs.gov/fdsnws/event/1/. If \code{obj@site} is something
other than "https://service.iris.edu", getEvent will point to \code{obj@site}/fdsnws/event/1/. The event service must be able to output
format=text.

}


\value{
A dataframe with the following columns:

\preformatted{
eventId ,time, latitude, longitude, depth, author, cCatalog, contributor,
  contributorId, magType, magnitude, magAuthor, eventLocationName 
}

Rows are ordered by \code{time}.

\strong{NOTE:} column names are identical to the names returned from the event web service
with the exception of "latitude" for "lat" and "longitude" for "lon". The longer names are
used for internal consistency -- all other web services return columns named "latitude" and "longitude".
}

\references{

The USGS event webservice:
  \url{https://earthquake.usgs.gov/fdsnws/event/1/}

}

\author{
Jonathan Callahan \email{jonathan@mazamascience.com}
}

%% \note{ }

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{IrisClient-class}},
}

\examples{
  \dontrun{
# NOTE:  'maps' and 'mapdata' packages must be installed
#require(maps)
#require(mapdata)

# Open a connection to IRIS DMC webservices
iris <- new("IrisClient")

# Get events > mag 5.0 over a week in June of 2012
starttime <- as.POSIXct("2012-06-21", tz="GMT")
endtime <- starttime + 3600 * 24 * 7
result <- try(events <- getEvent(iris, starttime, endtime, minmag=5.0))
if (inherits(result,"try-error")) {
  message(geterrmessage())
} else {

# Look at all events
  print(paste(nrow(events),"earthquakes found with magnitude > 5.0"))
  
# Plot events on a map
  #map('world')
  #points(events$longitude, events$latitude, pch=16, cex=1.5, col='red')
  #labels <- paste(" ", as.character(round(events$magnitude,1)), sep="")
  #text(events$longitude, events$latitude, labels=labels, pos=4, cex=1.2, col='red3')
  }
}
}
\keyword{webservices}
