\name{Trace-class}
\Rdversion{1.1}
\docType{class}
\alias{Trace-class}
\alias{Trace}
\alias{as.vector,Trace-method}
\alias{initialize,Trace-method}
\alias{isDC}
\alias{isDC,Trace-method}
\alias{plot,Trace-method}

%% NOTE:  the following lines suppress warning messages from R CMD check but clutter up the index

%%\alias{+,Trace,Trace-method}
%%\alias{as.vector,Trace-method}
%%\alias{plot,Trace-method}


\title{Class \code{"Trace"}}
\description{
A \code{Trace} object containing a seismic trace -- a continuous timeseries.
}

\section{Objects from the Class}{
Objects occupy the \code{traces} slot of a \link{Stream-class} object and are typically
populated by calls to \link{getDataselect}.
}

\section{Slots}{
  \describe{
    \item{\code{id}:}{Object of class \code{"character"}: 
    Unique "SNCL" identifier specifying the Network, Station, Location, Channel and Quality factor
    associated with this trace: eg. \code{AK.PIN..VEA.M}. The \code{id} is generated automatically
    when the trace is first created and is intended for read only.}
    \item{\code{Sensor}:}{Object of class \code{"character"}:
    Instrument name.}
    \item{\code{InstrumentSensitivity}:}{Object of class \code{"numeric"}:
    The total sensitivity for a channel, representing the complete acquisition system expressed as a scalar.
    Equivalent to SEED stage 0 gain.}
    \item{\code{SensitivityFrequency}:}{Object of class \code{"numeric"}:
    The frequency at which the total sensitivity is correct.}
    \item{\code{InputUnits}:}{Object of class \code{"character"}:
    The units of the data as input from the perspective of data acquisition.
    After correcting data for this response, these would be the resulting units.}
    \item{\code{stats}:}{Object of class \code{"TraceHeader"}: 
    Container with metadata information describing the trace. (see \link{TraceHeader-class}) }
    \item{\code{data}:}{Object of class \code{"numeric"}: Vector of data values. }
  }
}
  
\section{Methods}{
  \describe{
    \item{as.vector}{\code{signature(x="Trace")}: 
    returns the data slot; equivalent to \code{x@data} }
    \item{DDT}{\code{signature(x="Trace", demean="logical", detrend="logical", taper="numeric")}: 
    returns a new trace that has been 'cleaned up' for further processing by applying demean, detrend, and taper techniques (see \link{DDT}) }
    \item{envelope}{\code{signature(x="Trace")}: 
    returns the envelope of the seismic signal (see \link{envelope}) }
    \item{isDC}{\code{signature(x="Trace")}: 
    returns \code{TRUE} if trace data consist of a DC signal }
    \item{length}{\code{signature(x="Trace")}: 
    returns the length of the data; equivalent to \code{length(x@data)} }
    \item{max}{\code{signature(x="Trace")}: 
    returns the maximum value of the data; equivalent to \code{max(x@data)} }
    \item{median}{\code{signature(x="Trace", na.rm="logical")}: 
    returns the median value of the data; equivalent to \code{median(x@data)} }
    \item{mean}{\code{signature(x="Trace")}: 
    returns the mean value of the data; equivalent to \code{mean(x@data)} }
    \item{min}{\code{signature(x="Trace")}: 
    returns the minimum value of the data; equivalent to \code{min(x@data)} }
    \item{multiplyBy}{\code{signature(x="Trace", y="numeric")}: 
    returns a new \code{Trace} where the data have been multiplied by y (see \link{multiplyBy})}
    \item{plot}{\code{signature(x="Trace")}: 
    default plot of the \code{Trace} data with appropriate labeling}
    \item{rms}{\code{signature(x="Trace")}: 
    returns the Root Mean Square amplitude of the data (see \link{rms}) }
    \item{rmsVariance}{\code{signature(x="Trace")}: 
    returns the RMS variance of the data (see \link{rmsVariance}) }
    \item{sd}{\code{signature(x="Trace", na.rm="logical")}: 
    returns the standard deviation of the data; equivalent to \code{sd(x@data)} }
    \item{slice}{\code{signature(x="Trace", starttime="POSIXct", endtime="POSIXct")}: 
    returns a new \code{Trace} subset of an existing \code{Trace} (see \link{slice})}
    \item{STALTA}{\code{signature(x="Trace",staSecs="numeric",ltaSecs="numeric",algorithm="character", ...)}: 
    returns the STALTA picker result (see \link{STALTA})}
    \item{triggerOnset}{\code{signature(x="Trace", picker="numeric", threshold="numeric", ...)}: 
    returns the time or index of an event onset as determined by the STALTA picker (see \link{triggerOnset})}
	 }
}

%% \references{ }

\author{
Jonathan Callahan \email{jonathan@mazamascience.com}
}

\note{
The \code{Trace} object is inspired by the \code{Trace} class found in the
python ObsPy package (\url{https://docs.obspy.org/packages/autogen/obspy.core.trace.Trace.html}).
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%% \seealso{ }

\examples{
\dontrun{
# Open a connection to IRIS DMC webservices
iris <- new("IrisClient")

# Set the starttime and endtime
starttime <- as.POSIXct("2012-01-24", tz="GMT")
endtime <- as.POSIXct("2012-01-25", tz="GMT")

# Get the waveform
st <- getDataselect(iris,"AK","PIN","","BHZ",starttime,endtime)

# Get the first trace and generate some statistics
tr1 <- st@traces[[1]]
min(tr1)
median(tr1)
mean(tr1)
max(tr1)
sd(tr1)
rms(tr1)
rmsVariance(tr1)
}
}

\keyword{classes}
