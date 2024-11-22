\name{envelope}
\docType{methods}
\alias{envelope}
\alias{envelope,Trace-method}

\title{Envelope of a seismic signal}

\description{
The \code{envelope} method of \code{Trace} objects returns a \code{Trace}
whose data have been replaced with the envelope of the seismic signal.
}

\usage{
envelope(x)
}

\arguments{
  \item{x}{a \code{Trace} object}
}

\details{
Before calculating the envelope, the seismic trace is 'cleaned up' by removing the mean,
the trend and by applying a cosine taper.  See \link{DDT} for more details.

The seismic envelope is defined as:

\deqn{ E(t) = \sqrt{T^2(t) + H^2(t)} }

where \eqn{T(t)} is the seismic trace and \eqn{H(t)} is the Hilbert transform of \eqn{T(t)}.

}

\value{
A \code{Trace}
whose data have been replaced with the envelope of the seismic signal.
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

# Demean, detrend, cosine taper
tr <- DDT(tr)

# Create envelope version of the trace
trenv <- envelope(tr)

# Plot signal data and envelope data
plot(tr@data, type='l', col='gray80')
points(trenv@data, type='l', col='blue')
}
}

\keyword{methods}
