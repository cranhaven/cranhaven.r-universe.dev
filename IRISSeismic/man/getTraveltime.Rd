\name{getTraveltime}
\docType{methods}
\alias{getTraveltime}
\alias{getTraveltime,IrisClient,numeric,numeric,numeric,numeric,numeric-method}

\title{Retrieve seismic traveltime information from IRIS DMC}

\description{
The \code{getTraveltime} method obtains seismic traveltime data from
the IRIS DMC \code{traveltime} web service and returns it in a dataframe.
}

\usage{
getTraveltime(obj, latitude, longitude, depth, staLatitude, staLongitude)
}

\arguments{
  \item{obj}{ an \code{IrisClient} object }
  \item{latitude}{ latitude of seismic event }
  \item{longitude}{ longitude of seismic event }
  \item{depth}{ depth of seismic event }
  \item{staLatitude}{ latitude of seismic station }
  \item{staLongitude}{ longitude of seismic station }
}

\details{
The \code{traveltime} web service calculates travel-times for seismic phases using a 1-D spherical earth model.
}


\value{
A dataframe with the following columns:

%% max preformatted length ---------------------------------------------------
\preformatted{
distance, depth, phaseName, travelTime, rayParam, takeoff, incident
  puristDistance, puristName
}

Rows are ordered by \code{travelTime}.
}

\references{
The IRIS DMC traveltime web service:

  \url{https://service.iris.edu/irisws/traveltime/1/}
}

\author{
Jonathan Callahan \email{jonathan@mazamascience.com}
}

%% \note{ }

\seealso{
\code{\link{IrisClient-class}}
}

\examples{
\dontrun{
# Open a connection to IRIS DMC webservices
iris <- new("IrisClient")

# Two days around the "Nisqually Quake"
starttime <- as.POSIXct("2001-02-27", tz="GMT")
endtime <- starttime + 3600 * 24 * 2

# Find biggest seismic event over these two days -- it's the "Nisqually"
events <- getEvent(iris, starttime, endtime, minmag=5.0)
bigOneIndex <- which(events$magnitude == max(events$magnitude))
bigOne <- events[bigOneIndex[1],]

# Find US stations that are available within an hour of the event
start <- bigOne$time
end <- start + 3600
availability <- getAvailability(iris, "US", "", "", "BHZ",
                                starttime=start, endtime=end,
                                latitude=bigOne$latitude, longitude=bigOne$longitude,
                                minradius=0, maxradius=10)
    
# Get the station the furthest East
minLonIndex <- which(availability$longitude == max(availability$longitude))
snclE <- availability[minLonIndex,]

# Plot the BHZ signal from this station
st <- getDataselect(iris,snclE$network,snclE$station,snclE$location,snclE$channel,
                    start,end)

# Check that there is only a single trace and then plot it
length(st@traces)
tr <- st@traces[[1]]
plot(tr, subsampling=1) # need subsmpling=1 to add vertical lines with abline()

# Find travel times to this station
traveltimes <- getTraveltime(iris, bigOne$latitude, bigOne$longitude, bigOne$depth,
                             snclE$latitude, snclE$longitude)

# Look at the list                             
traveltimes

# mark the P and S arrival times
pArrival <- start + traveltimes$travelTime[traveltimes$phaseName=="P"]
sArrival <- start + traveltimes$travelTime[traveltimes$phaseName=="S"] 
abline(v=pArrival, col='red')
abline(v=sArrival, col='blue')
}
}

\keyword{webservices}
