\name{metricList2Xml}

\alias{metricList2Xml}


\title{Create XML for the BSS}

\description{
The \code{metricList2Xml} function converts a list of \code{SingleValueMetric}s
or \code{GeneralValueMetric} into an
XML structure appropriate for submitting to the MUSTANG Backend Storage System (BSS).
}

\usage{
metricList2Xml(metricList)
}

\arguments{
  \item{metricList}{a list of \code{SingleValueMetric} or \code{GeneralValueMetric} objects.}
}

\details{
Metrics functions return lists of \code{SingleValueMetric} or \code{GeneralValueMetric} objects.  A long \code{metricList} may be built up
by appending the results of different metrics functions or the same metrics function operating on different seismic signals.
The list may only contain a single class (\code{SingleValueMetric} cannot be mixed with \code{GeneralValueMetric} objects).
These metrics can be submitted to the BSS in a standardized XML format. (see \link{SingleValueMetric-class})
}

\value{
A character string with BSS formatted XML is returned.
}

%% \references{ }

\author{
Jonathan Callahan \email{jonathan@mazamascience.com}
}

%% \note{ }

%% ~Make other sections like Warning with \section{Warning }{....} ~

%% \seealso{ }

\examples{
  \dontrun{
# Open a connection to IRIS DMC webservices
iris <- new("IrisClient")

# Get the waveform
starttime <- as.POSIXct("2012-01-24", tz="GMT")
endtime <- as.POSIXct("2012-01-25", tz="GMT")
st <- getDataselect(iris,"AK","PIN","","BHZ",starttime,endtime)

# Apply a metric and show the results
metricList <- stateOfHealthMetric(st)
metricList <- append(metricList, basicStatsMetric(st))
bssXml <- metricList2Xml(metricList)
  }
}

