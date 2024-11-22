\name{createBssUrl}

\alias{createBssUrl}
\alias{createBssUrl,IrisClient,character,character,character,character,POSIXct,POSIXct,character-method}

\title{Create URL to retrieve measurements from the MUSTANG BSS }

\description{
The \code{createBssUrl} method of the \code{IrisClient} returns a URL that can be used to make
a request of the MUSTANG BSS (Backend Storage System).
}

\usage{
createBssUrl(obj, network, station, location, channel, 
             starttime, endtime, metricName, ...)
}

\arguments{
  \item{obj}{ an \code{IrisClient} object }
  \item{network}{ a character string with the two letter seismic network code }
  \item{station}{ a character string with the station code }
  \item{location}{ a character string with the location code }
  \item{channel}{ a character string with the three letter channel code }
  \item{starttime}{ a POSIXct class specifying the starttime (GMT) }
  \item{endtime}{ a POSIXct class specifying the endtime (GMT) }
  \item{metricName}{ a character string containing one or more comma separated metric names }
  \item{\dots}{optional arguments
    \code{constraint}{ a character string containing value constraints }
    \code{url}{ optional url of the BSS measurements service }
  }
}

\details{

A blank location code should be specified as \code{location="--"}; Using \code{location=""} will return all location codes.

The default MUSTANG measurement service when \code{url} is not specified is:

\code{http://service.iris.edu/mustang/measurements/1/query?}

}

\value{
A character string containing a BSS request URL
}

%% \references{ }

\author{
Jonathan Callahan \email{jonathan@mazamascience.com}
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{getSingleValueMetrics}}
}

%% max preformatted length ---------------------------------------------------
\examples{
# Open a connection to IRIS DMC webservices (including the BSS)
iris <- new("IrisClient", debug=TRUE)

starttime <- as.POSIXct("2013-06-01", tz="GMT")
endtime <- starttime + 30*24*3600
metricName <- "sample_max,sample_min,sample_mean"

# Get the measurement dataframe
url <- createBssUrl(iris,"IU","ANMO","00","BHZ",
                    starttime,endtime,metricName)

# This URL can be pasted into a web browser to see the BSS return values
}

\keyword{ webservices }
