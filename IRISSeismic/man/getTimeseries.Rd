\name{getTimeseries}

\alias{getTimeseries}
\alias{getTimeseries,IrisClient,character,character,character,character,POSIXct,POSIXct-method}

\title{Retrieve seismic data from IRIS DMC}

\description{
The \code{getTimeseries} method makes a request of the IRIS DMC \code{timeseries} 
webservice and returns a \code{Stream} object in which individual \code{Traces}
have been sorted by start time.
}

\usage{
getTimeseries(obj, network, station, location, channel, 
              starttime, endtime,...)
}

\arguments{
  \item{obj}{\code{IrisClient} object }
  \item{network}{character string with the two letter seismic network code }
  \item{station}{character string with the station code }
  \item{location}{character string with the location code }
  \item{channel}{character string with the three letter channel code }
  \item{starttime}{POSIXct class specifying the starttime (GMT) }
  \item{endtime}{POSIXct class specifying the endtime (GMT) }
  \item{\dots}{optional arguments
     \code{repository} optional character string identifying whether to exclusively search primary archive or realtime collection buffers.
           Acceptable values are "primary" or "realtime". If not specified, IRIS webservices defaults to both repositories.
     \code{processing}{optional character string with processing commands }
     \code{inclusiveEnd} optional logical determining whether the endtime is inclusive (default = TRUE)
     \code{ignoreEpoch} optional logical defining behavior when multiple epochs are encountered (default = FALSE)
   }
}

\details{
This is an alternative method for retreiving seismic data that accepts optional processing commands.
Data requests are made through
the \code{timeseries} webservice and returned data are parsed using
the internal miniseed2Stream() function.

If the location argument contains an empty string to specify a 'blank' location code, a location
code of \code{"--"} will be used in the dataselect request URL.

The \code{processing} parameter can be used to specify any type of processing supported by the
\code{timeseries} webs service.  This string must begin with an ampersand and be ready to be appended to the request url, e.g.
\code{processing="&demean=true&taper0.2,HANNING"}.  This gives the user complete control over the number
and order of processing commands.
(See \href{https://service.iris.edu/irisws/timeseries/1/}{timeseries documentation}.)
  
If \code{inclusiveEnd=FALSE}, then \code{getDataselect} will subtract 0.000001 seconds from the endtime before passing the 
value to the \code{dataselect} webservice. An endtime of, e.g., \code{as.POSIXct("2016-01-03", tz="GMT")} will be passed 
into \code{dataselect} as \code{end=2016-01-02T23:59:59.999999}. A data sample at time 2016-01-03T00:00:00 will not be returned
unless \code{inclusiveEnd=TRUE}.

Sometimes the station webservice will return multiple records for the same SNCL,
each with a different scale or starttime.  These represent different \emph{epochs} with
potentially different metadata parameters for the SNCL and, by default, will cause
a 'Multiple epochs' error message to be generated.

Handling all possible metadata differences so that the data may be merged is beyond the scope of this
package. Instead, to avoid errors, users may specify \code{ignoreEpoch=TRUE} in which case
the very first SNCL-epoch encountered will be used and all others will be discarded.

For access to restricted data, getDataselect will look for system environmental variable "IrisClient_netrc"
which should point to a .netrc authentication file.

Error returns from the webservice will stop evaluation and generate an error message.
}

\value{
A new \code{Stream} object is returned.
}

\references{
%% ~put references to the literature/web site here ~
The IRIS DMC timeseries webservice:

  \url{https://service.iris.edu/irisws/timeseries/1/}
}

\author{
Jonathan Callahan \email{jonathan.s.callahan@gmail.com}
}

%% \note{ }

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{getDataselect}}, 
\code{\link{getSNCL}}, 
\code{\link{IrisClient-class}}
}

\examples{
\dontrun{
# Open a connection to IRIS DMC webservices (use debug=TRUE so we can see the URLs generated)
iris <- new("IrisClient",debug=TRUE)

starttime <- as.POSIXct("2013-06-01",tz="GMT")
endtime <- starttime + 24*3600

# Get raw data and processed data
st1 <- getDataselect(iris,"IU","ANMO","00","LHZ",starttime,endtime)
st2 <- getTimeseries(iris,"IU","ANMO","00","LHZ",starttime,endtime,"&demean=true&taper=0.2,HANNING")

layout(matrix(seq(2)))
plot(st1)
plot(st2)
}
}

\keyword{webservices}

