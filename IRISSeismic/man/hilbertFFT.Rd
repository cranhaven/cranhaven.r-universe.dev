\name{hilbertFFT}

\alias{hilbertFFT}

\title{Hilbert FFT}

\description{
The \code{hilbertFFT} function returns the complex Hilbert FFT of a timeseries signal.
}

\usage{
hilbertFFT(x)
}

\arguments{
  \item{x}{a numeric vector}
}

\details{
This function is intended for internal use by the hilbert() and envelope() methods of 
\code{Trace} objects.
}

\value{
A complex vector containing the Hilbert FFT of \code{x}.
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

# Calculate Hilbert FFT of the trace data
hfft <- hilbertFFT(tr@data)

# Plot signal, with Hilbert envelope
layout(1)
plot(tr@data, type='l', col='gray80', main="Signal and Envelope")
points(Mod(hfft), type='l', col='blue')

# 2 rows
layout(matrix(seq(2)))

# Show that Imaginary component of Hilbert transform has the
# original signal shifted by 90 degrees
ccf(tr@data,tr@data,lag.max=200,main="Auto-correlation of signal data")
ccf(tr@data,Im(hfft),lag.max=200,main="90 deg phase shift with Hilber transform")

# Restore default layout
layout(1)
}
}

\keyword{methods}
