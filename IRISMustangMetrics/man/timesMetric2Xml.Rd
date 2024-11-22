\name{timesMetric2Xml}

\alias{timesMetric2Xml}


\title{Create XML for the BSS}

\description{
The \code{timesMetric2Xml} function converts a \code{MultipleTimeValueMetric} into an
XML structure appropriate for submitting to the MUSTANG Backend Storage System (BSS).
}

\usage{
timesMetric2Xml(metric)
}

\arguments{
  \item{metric}{a \code{MultipleTimeValueMetric} object}
}

%% \details{ }

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

# Make sure we're working with a single snclq
unique_ids <- uniqueIds(st)
if (length(unique_ids) > 1) {
  stop(paste("meanMetric: Stream has",unique_ids,"unique identifiers"))
}
snclq <- unique_ids[1]

# get the upDownTimes with a minimum signal length and minimum gap (secs)
upDownTimes <- getUpDownTimes(st, min_signal=30, min_gap=60)
  
# Create and return a MultipleTimeValue metric from the upDownTimes
m <- new("MultipleTimeValueMetric", snclq=snclq, starttime=starttime, 
         endtime=endtime, metricName="up_down_times", values=upDownTimes)

# Show the XML version of the metric
bssXml <- timesMetric2Xml(m)
cat(bssXml)
  }
}


