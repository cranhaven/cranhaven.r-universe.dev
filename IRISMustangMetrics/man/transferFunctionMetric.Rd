\name{transferFunctionMetric}

\alias{transferFunctionMetric}

\title{Cross-spectral comparison}

\description{
The transferFunctionMetric() function calculates metrics that assess the relationship between
two SNCLs with the same network, station and channel but separate locations.  When seismometers are
working properly, the transfer function amplitude and phase will match similar values calculated
from the instrument responses.

This function calculates the transfer function from data in the incoming streams.
Response information is then obtained from the \href{http://service.iris.edu/irisws/evalresp/1/}{evalresp web service}.
}

\usage{
transferFunctionMetric(st1, st2, evalresp1, evalresp2)
}

\arguments{
  \item{st1}{a \code{Stream} object containing a seismic signal}
  \item{st2}{a \code{Stream} object containing a seismic signal}
  \item{evalresp1}{a \code{data frame} containing an amplitude and phase spectrum}
  \item{evalresp2}{a \code{data frame} containing an amplitude and phase spectrum}
}

\details{
%% max preformatted length ---------------------------------------------------
Details of the algorithm are as follows
\preformatted{
# compute complex cross-spectrum of traces x and y ==> Pxx, Pxy, Pyy
# calculate transfer function values:
#     Txy(f) = Pxy(f) / Pxx(f)
#     dataGain <- Mod(Txy)
#     dataPhase <- Arg(Txy)
#
# calculate avgDataGain and avgDataPhase values for periods of 5-7s
#
# calculate the corresponding response amplitude ratio and phase difference:
#     request responses for x and y
#     respGain = respGainy(f) / respGainx(f)
#     respPhase = respPhasey(f) - respPhasex(f)
#
# calculate avgRespGain and avgRespPhase values for periods of 5-7s
#
# calculate metrics:
#     gain_ratio = avgDataGain / avgRespGain
#     hase_diff = avgDataPhase - avgRespPhase
#     ms_coherence = |Pxy|^2 / (Pxx*Pyy)
}
}

\value{
A list with a single \code{SingleValueMetric} object is returned.  The metric name is
\code{transfer_function} and it has three attributes:
\itemize{
  \item{\code{gain_ratio} -- reasonableness of cross-spectral amplitude between \code{st1} and \code{st2}}
  \item{\code{phase_diff} -- reasonableness of cross-spectral phase between \code{st1} and \code{st2}}
  \item{\code{ms_coherence} -- mean square coherence between \code{st1} and \code{st2}}
}
These values can be interpreted as follows:

Whenever \code{ms_coherence ~= 1.0}, properly functioning seismometers should have:
\itemize{
  \item{\code{gain_raio ~= 1.0}}
  \item{\code{phase_diff < 10.0} (degrees)}
}
}

%% \references{ }

\author{
Jonathan Callahan \email{jonathan@mazamascience.com} (R code),
Mary Templeton \email{mary.templeton@earthscope.org} (algorithm)
}

\note{
Seismic streams passed to transferFunctionMetric() must have the same network, station
and channel and must cover the same time range. The two channels should also have values of
azimuth and dip within five degrees of each other.  If sampling rates differ and one is a multiple
of the other, the stream with the higher sampling rate will be decimated to match the lower sampling rate.

The metricList generated for these two-channel metrics will have a SNCL code of the form:
\code{N.S.L1:L2.C.Q}.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%% \seealso{ }

\examples{
  \dontrun{
# Create a new IrisClient
iris <- new("IrisClient", debug=TRUE)

# Get seismic data
starttime <- as.POSIXct("2011-05-01 12:00:00", tz="GMT")
endtime <- starttime + 3600

st1 <- getDataselect(iris,"CI","PASC","00","BHZ",starttime,endtime,inclusiveEnd=FALSE)
st2 <- getDataselect(iris,"CI","PASC","10","BHZ",starttime,endtime,inclusiveEnd=FALSE)
evalresp1 <- IRISSeismic::transferFunctionSpectra(st1,40)
evalresp2 <- IRISSeismic::transferFunctionSpectra(st2,40)

# Calculate metrics
metricList <- transferFunctionMetric(st1,st2,evalresp1,evalresp2)
print(metricList)
  }
}

\keyword{metrics}
