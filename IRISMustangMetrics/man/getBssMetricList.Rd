\name{getBssMetricList}

\alias{getBssMetricList}
\alias{getBssMetricList,IrisClient,character,character,character,character,POSIXct,POSIXct,character,character-method}
\alias{getBssMetricList,IrisClient,character,character,character,character,POSIXct,POSIXct,character,missing-method}

\title{Retrieve measurements XML from the MUSTANG BSS and convert them to a metricList}

\description{
The \code{getBssMetricList} method makes a request of the MUSTANG BSS (Backend Storage System)
and returns a list of \code{_Metric} objects.
}

\usage{
getBssMetricList(obj, network, station, location, channel, 
                 starttime, endtime, metricName, url)
}

\arguments{
  \item{obj}{ an \code{IrisClient} object }
  \item{network}{ a character string with the two letter seismic network code }
  \item{station}{ a character string with the station code }
  \item{location}{ a character string with the location code }
  \item{channel}{ a character string with the three letter channel code }
  \item{starttime}{ a POSIXct class specifying the starttime (GMT) }
  \item{endtime}{ a POSIXct class specifying the endtime (GMT) }
  \item{metricName}{ a character string identifying the name of the metric stored in the BSS }
  \item{url}{ optional url of the BSS measurements service }
}

\details{
This method calls on \link{getMetricsXml} to communicate with the BSS and obtain an XML reponse.
This response is then processed and used to create \code{_Metric} objects which are returned as a metricList.

Error returns from the BSS will stop evaluation and throw an error message.
}

\value{
A list of \code{_Metric} objects is returned.
}

%% \references{ }

\author{
Jonathan Callahan \email{jonathan@mazamascience.com}
}

%% \note{ }

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{getMetricsXml}}
}

\examples{
  \dontrun{
# Open a connection to IRIS DMC webservices (including the BSS)
iris <- new("IrisClient", debug=TRUE)

starttime <- as.POSIXct("2014-01-24", tz="GMT")
endtime <- as.POSIXct("2014-01-25", tz="GMT")

# Get the metricList
metricList <- getBssMetricList(iris,"AK","PIN","","",starttime,endtime,
                               metricName="sample_mean")
show(metricList)
  }
}

\keyword{ webservices }
