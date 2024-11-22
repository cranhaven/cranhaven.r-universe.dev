\name{hilbert}
\docType{methods}
\alias{hilbert}
\alias{hilbert,Trace-method}


\title{Hilbert of a seismic signal}

\description{
The \code{hilbert} method of \code{Trace} objects returns a \code{Trace}
whose data have been replaced with the Hilbert transform of the seismic signal.
}

\usage{
hilbert(x)
}

\arguments{
  \item{x}{a \code{Trace} object}
}

\details{
Before calculating the Hilbert transform, the seismic trace is 'cleaned up' by removing the mean,
the trend and by applying a cosine taper.  See \link{DDT} for more details.
}

\value{
A \code{Trace}
whose data have been replaced with the Hilbert transform of the seismic signal.
}


\author{
Jonathan Callahan \email{jonathan@mazamascience.com}
}

\note{
This algorithm is adapted from code in the \pkg{seewave} package.
}

\examples{
\dontrun{
# Open a connection to IRIS DMC webservices
iris <- new("IrisClient")

starttime <- as.POSIXct("2010-02-27 06:00:00", tz="GMT")
endtime <- as.POSIXct("2010-02-27 09:00:00", tz="GMT")

# Get the waveform
st <- getDataselect(iris,"IU","ANMO","00","BHZ",starttime,endtime)
tr <- st@traces[[1]]

# Create Hilbert transform of the trace
trh <- hilbert(tr)

# Plot signal data and hilbert data
plot(tr@data, type='l', col='gray80')
points(trh@data, type='l', col='blue')
}
}

\keyword{methods}
