\name{saveMetricList}

\alias{saveMetricList}

\title{Save a MetricList as RData or XML}

\description{
The saveMetricList() function allows metrcis to be saved as either .RData files or as XML.
The XML format is the same as that used by the IRIS DMC MUSTANG database for metric submission.
}

\usage{
saveMetricList(metricList, id=Sys.getpid(), rdata=FALSE)
}

\arguments{
  \item{metricList}{list of SingleValueMetric objects}
  \item{id}{ID to be used when generating output files}
  \item{rdata}{optional flag to save the incoming \code{metricList} as a .RData file}
}

\details{
The \code{saveMetricList} function saves a list of \code{SingleValueMetrics} as a .RData binary file
or converts the list into the XML format expected by the MUSTANG database submission process. This XML
format is human readable and can be used to spot check results of metrics calculations.
}

\value{
The automatically generated filename is returned invisibly.
}

%% \references{ }

\author{
Jonathan Callahan \email{jonathan@mazamascience.com}
}

%% \note{ }

%% ~Make other sections like Warning with \section{Warning}{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
\code{\link{SingleValueMetric-class}},
\code{\link{metricList2Xml}},
\code{\link{getMetricsXml}},
\code{\link{getBssMetricList}},
}

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
saveMetricList(metricList,id='AK.PIN..BHZ')
  }
}

