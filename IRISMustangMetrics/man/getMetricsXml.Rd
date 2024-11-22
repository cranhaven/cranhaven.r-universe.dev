\name{getMetricsXml}

\alias{getMetricsXml}
\alias{getMetricsXml,IrisClient,character,character,character,character,POSIXct,POSIXct,character,character-method}
\alias{getMetricsXml,IrisClient,character,character,character,character,POSIXct,POSIXct,character,missing-method}

\title{ Retrieve measurements XML from the MUSTANG BSS }

\description{
The \code{getMetricsXml} method makes a request of the MUSTANG BSS (Backend Storage System)
and returns a character string with the response XML.
}

\usage{
getMetricsXml(obj, network, station, location, channel, 
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
The default BSS measurement service when \code{url} is not specified is:

\code{http://service.iris.edu/mustang/measurements/1/query?}

This method returns raw XML which is not that useful by itself.  Users should instead use the
\link{getBssMetricList} method which calls this function and returns a list \code{_Metric} objects.

Error returns from the BSS will stop evaluation and throw an error message.
}

\value{
A character string with the XML response from the BSS is returned.
}

%% \references{ }

\author{
Jonathan Callahan \email{jonathan@mazamascience.com}
}

%% \note{ }

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{getBssMetricList}}
}

\examples{
# Open a connection to IRIS DMC webservices (including the BSS)
iris <- new("IrisClient", debug=TRUE)

starttime <- as.POSIXct("2012-01-24", tz="GMT")
endtime <- as.POSIXct("2012-01-25", tz="GMT")

# Get the measurement XML
xml <- tryCatch(getMetricsXml(iris,"AK","PIN","","BHZ",
                          starttime,endtime,metricName="sample_mean",
                          url="http://service.iris.edu/mustang/measurements/1/query?"),
                error= function(e) {message(e)}) 
}

\keyword{ webservices }
