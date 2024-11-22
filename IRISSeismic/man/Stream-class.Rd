\name{Stream-class}
\Rdversion{1.1}
\docType{class}
\alias{Stream-class}
\alias{Stream}
\alias{plot,Stream-method}
\alias{plotUpDownTimes}
\alias{plotUpDownTimes,Stream,missing,missing-method}
\alias{plotUpDownTimes,Stream,missing,numeric-method}
\alias{plotUpDownTimes,Stream,numeric,missing-method}
\alias{plotUpDownTimes,Stream,numeric,numeric-method}
\alias{plotUpDownTimes,POSIXct,missing,missing-method}
\alias{plotUpDownTimes,POSIXct,missing,numeric-method}
\alias{plotUpDownTimes,POSIXct,numeric,missing-method}
\alias{plotUpDownTimes,POSIXct,numeric,numeric-method}
\alias{slice,Stream,POSIXct,POSIXct-method}
\alias{uniqueIds}
\alias{uniqueIds,Stream-method}

\title{ Class \code{"Stream"} }

\description{
A \code{Stream} object containing a list of \code{Trace} objects.
}

\section{Objects from the Class}{
Objects are typically created by calls to \link{getDataselect}.
}

\section{Slots}{
  \describe{
    \item{\code{url}:}{Object of class \code{"character"}:
    URL request used to generate this \code{Stream}. }
    \item{\code{requestedStarttime}:}{Object of class \code{"POSIXct"}:
    starttime used when requesting data with \code{\link{getDataselect}}. }
    \item{\code{requestedEndtime}:}{Object of class \code{"POSIXct"}:
    endtime used when requesting data with \code{\link{getDataselect}}. }
    \item{\code{act_flags}:}{Object of class \code{"integer"}:
    Accumulators for the act_flags bits in each miniSEED record. }
    \item{\code{io_flags}:}{Object of class \code{"integer"}:
    Accumulators for the io_flags bits in each miniSEED record. }
    \item{\code{dq_flags}:}{Object of class \code{"integer"}:
    Accumulators for the dq_flags bits in each miniSEED record. }
    \item{\code{timing_qual}:}{Object of class \code{"numeric"}:
    Average timing quality associated with miniSEED records.}
    \item{\code{traces}:}{Object of class \code{"list"}:
    List of \code{Trace} objects. }
  }
}

\section{Methods}{
  \describe{
    \item{getGaps}{\code{signature(x="Stream")}: 
    returns information on data dropouts between \code{Traces}; see \link{getGaps} }
    \item{getUpDownTimes}{\code{signature(x="Stream", min_signal="numeric", min_gap="numeric")}: 
    returns a vector of datetimes associated with channel up/down transitions; see \link{getUpDownTimes} }
    \item{length}{\code{signature(x="Stream")}: 
    returns the total number of data points in all \code{Traces} }
    \item{max}{\code{signature(x="Stream")}: 
    returns the overall data maximum for all data in all \code{Traces} }
    \item{median}{\code{signature(x="Stream", na.rm= "logical")}: 
    returns the overall data median for all data in all \code{Traces} }
    \item{mean}{\code{signature(x="Stream")}: 
    returns the overall data mean for all data in all \code{Traces} }
    \item{mergeTraces}{\code{signature(x="Stream", fillMethod="fillNA")}: 
    returns a new \code{Stream} object where all \code{Traces} have been merged into a single \code{Trace} \link{mergeTraces} }
    \item{min}{\code{signature(x="Stream")}: 
    returns the overall data minimum for all data in all \code{Traces} }
    \item{multiplyBy}{\code{signature(x="Stream", y="numeric")}: 
    returns a new \code{Stream} object where the data in every \code{Trace} have been multiplied by y; see \link{multiplyBy} }
    \item{parallelLength}{\code{signature(x="Stream")}: 
    returns a vector of data lengths, one for each \code{Trace} }
    \item{parallelMax}{\code{signature(x="Stream")}: 
    returns a vector of data maxima, one for each \code{Trace} }
    \item{parallelMedian}{\code{signature(x="Stream", na.rm= "logical")}: 
    returns a vector of data medians, one for each \code{Trace} }
    \item{parallelMean}{\code{signature(x="Stream")}: 
    returns a vector of data means, one for each \code{Trace} }
    \item{parallelMin}{\code{signature(x="Stream")}: 
    returns a vector of data minima, one for each \code{Trace} }
    \item{parallelRms}{\code{signature(x="Stream")}: 
    returns a vector of RMS calculations, one for each \code{Trace}; see \link{rmsVariance} }
    \item{parallelRmsVariance}{\code{signature(x="Stream")}: 
    returns a vector of RMS variance calculations, one for each \code{Trace}; see \link{rmsVariance} }
    \item{parallelSd}{\code{signature(x="Stream", na.rm="logical")}: 
    returns a vector of standard deviation calculations, one for each \code{Trace} }
    \item{plot}{\code{signature(x="Stream")}: 
    default plot of the merged \code{Traces} in a \code{Stream} with appropriate labeling}
    \item{plotUpDownTimes}{\code{signature(x="Stream", min_signal="numeric", min_gap="numeric")}: 
    plots the times at which a \code{Stream} transitions from data collection to non-collection (on/off); see \link{getUpDownTimes}}
    \item{rms}{\code{signature(x="Stream")}: 
    returns the overall Root Mean Square amplitude for all data in all \code{Traces}; see \link{rmsVariance} }
    \item{rmsVariance}{\code{signature(x="Stream")}: 
    returns the overall RMS variance for all data in all \code{Traces}; see \link{rmsVariance} }
    \item{sd}{\code{signature(x="Stream", na.rm="logical")}: 
    returns the overall standard deviations for all data in all \code{Traces} }
    \item{slice}{\code{signature(x="Stream", starttime="POSIXct", endtime="POSIXct")}: 
    returns a new \code{Stream} sliced out of an existing \code{Stream} (see \link{slice})}
    \item{uniqueIds}{\code{signature(x="Stream")}: 
    returns a vector of SNCLQ identifiers, one for each \code{Trace} }
	 }
}

%% \references{ }

\author{
Jonathan Callahan \email{jonathan@mazamascience.com}
}
\note{
The \code{Stream} object is inspired by the \code{Stream} class found in the
python ObsPy package (\url{https://docs.obspy.org/packages/autogen/obspy.core.stream.Stream.html}).

The miniSEED flags and timing_qual values are described in the SEED manual
(\url{http://www.fdsn.org/seed_manual/SEEDManual_V2.4.pdf}).  The "accumulators" contain
counts of the number of times each bit flag was set during the parsing of a miniSEED file.
These attributes are retained primarily for assessing data quality issues within the IRIS DMC.

The following code documentation describes how each of the flags is used within miniSEED files:
\preformatted{
  # act_flags
  # [1] Calibration signals present
  # [2] Time correction applied
  # [3] Beginning of an event, station trigger
  # [4] End of an event, station detrigger
  # [5] A positive leap second happened in this record
  # [6] A negative leap second happened in this record
  # [7] Event in progress
  # [8] Undefined bit set

  # io_flags
  # [1] Station volume parity error possibly present
  # [2] Long record read (possibly no problem)
  # [3] Short record read (record padded)
  # [4] Start of time series
  # [5] End of time series
  # [6] Clock locked
  # [7] Undefined bit set
  # [8] Undefined bit set

  # dq_flags
  # [1] Amplifier saturation detected
  # [2] Digitizer clipping detected
  # [3] Spikes detected
  # [4] Glitches detected
  # [5] Missing/padded data present
  # [6] Telemetry synchronization error
  # [7] A digital filter may be charging
  # [8] Time tag is questionable
}
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%% \seealso{ }

\examples{
\dontrun{
# Open a connection to IRIS DMC webservices
iris <- new("IrisClient")

starttime <- as.POSIXct("2012-01-24", tz="GMT")
endtime <- as.POSIXct("2012-01-25", tz="GMT")

# Get the waveform
st <- getDataselect(iris,"AK","PIN","","BHZ",starttime,endtime)
min(st)
median(st)
mean(st)
max(st)
sd(st)
rms(st)
rmsVariance(st)
}
}

\keyword{classes}
