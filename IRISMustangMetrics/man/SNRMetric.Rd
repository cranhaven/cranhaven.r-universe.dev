\name{SNRMetric}

\alias{SNRMetric}

\title{Signal to Noise Ratio}

\description{
The SNRMetric() function calculates the Signal-to-Noise Ratio of a seismic trace by one
of several named algorithms.
}

\usage{
SNRMetric(st, algorithm, windowSecs)
}

\arguments{
  \item{st}{ a \code{Stream} object containing a seismic signal }
  \item{algorithm}{ a named algorithm to use for calculating SNR (default=\code{"splitWindow"}) }
  \item{windowSecs}{ width (seconds) of the full window used in SNR calculations (default=\code{60}) }
}

\details{
Seismic signals in the Stream must be without gaps, \emph{i.e.} contained within a single Trace.

\strong{\code{algorithm="splitWindow"}}

This algorithm uses the midpoint of the seismic signal as the border between noise to the left
of the midpoint and signal to the right.
The value for signal-to-noise is just the rmsVariance calculated for 
windowSecs/2 seconds of data to the right of the midpoint divided by the rmsVariance
for windowSecs/2 seconds of data to the left of the midpoint.

No other algorithms have been vetted at this point.
%% The P-wave onset is determined with the following code:
%% \preformatted{
%%   picker <- STALTA(tr,3,30,"classic_LR")
%%   to <- triggerOnset(tr,picker)
%% }
%% 
%% Two separate traces are cut from the full trace on either side of the triggerOnset.  The size
%% of each is \code{windowSecs/2}.  After each has been demeaned and detrended, the
%% rmsVariance of each trace is calculated.  The final SNR value
%% is simply the ratio of the rmsVariance of the trace after P-wave onset to the rmsVariance
%% of the trace before P-wave onset.  This SNR value is returned as the following metric:
%% \itemize{
%%   \item \code{sample_snr}
%% }
}

\value{
A list with a single \code{SingleValueMetric} object is returned.
}

%% \references{ }

\author{
Jonathan Callahan \email{jonathan@mazamascience.com}
}

%% \note{
%% See the \code{seismic} package for documentation on \code{Trace} objects and the \code{STALTA} method.
%% }

%% \seealso{ }

\examples{
 \dontrun{
# Open a connection to IRIS DMC webservices
iris <- new("IrisClient")

# Get an hour long waveform centered on a big quake
starttime <- as.POSIXct("2010-02-27 06:16:15",tz="GMT")
endtime <- as.POSIXct("2010-02-27 07:16:15",tz="GMT")
st <- getDataselect(iris,"IU","ANMO","00","BHZ",starttime,endtime)
tr <- st@traces[[1]]

# Calculate the SNR metric and show the results
metricList <- SNRMetric(st)
dummy <- lapply(metricList, show)
  }
}

\keyword{metrics}
