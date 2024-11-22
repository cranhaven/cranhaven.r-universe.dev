\name{getPsdMetrics}

\alias{getPsdMetrics}
\alias{getPsdMetrics,IrisClient,character,character,character,character,POSIXct,POSIXct,character-method}
\alias{getPsdMetrics,IrisClient,character,character,character,character,POSIXct,POSIXct,missing-method}

\title{Retrieve measurements from the MUSTANG BSS }

\description{
The \code{getPsdMetrics} method of the \code{IrisClient} makes a request of the MUSTANG BSS (Backend Storage System)
and returns a dataframe containing instrument corrected Power Spectral Density (PSD) measurements.
}

\usage{
getPsdMetrics(obj, network, station, location, channel, starttime, endtime, url) 
}

\arguments{
  \item{obj}{ an \code{IrisClient} object }
  \item{network}{ a character string with the two letter seismic network code }
  \item{station}{ a character string with the station code }
  \item{location}{ a character string with the location code }
  \item{channel}{ a character string with the three letter channel code }
  \item{starttime}{ a POSIXct class specifying the starttime (GMT) }
  \item{endtime}{ a POSIXct class specifying the endtime (GMT) }
  \item{url}{ optional url of the BSS measurements service }
}

\details{
The default BSS measurement service when \code{url} is not specified is:

\code{http://service.iris.edu/mustang/noise-psd/1/query?}

Data returned from the BSS are converted into an \R dataframe.

Error returns from the BSS will stop evaluation and generate an error message.
}

\value{
A dataframe with the following columns:

\preformatted{
target, starttime, endtime, frequency, power
}
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
  \dontrun{
# Open a connection to IRIS DMC webservices (including the BSS)
iris <- new("IrisClient", debug=TRUE)

starttime <- as.POSIXct("2012-01-24", tz="GMT")
endtime <- as.POSIXct("2012-01-25", tz="GMT")

# Get the measurement XML
psdDF <- getPsdMetrics(iris,"AK","PIN","","BHZ", starttime,endtime)
  }
}

\keyword{ webservices }
