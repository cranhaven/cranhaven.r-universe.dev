\name{getMustangMetrics}

\alias{getMustangMetrics}
\alias{getMustangMetrics,IrisClient,character,character,character,character,POSIXct,POSIXct,character-method}

\title{Retrieve measurements from the MUSTANG BSS }

\description{
The \code{getMustangMetrics} method of the \code{IrisClient} makes a request of the MUSTANG database
and returns a dataframe containing metrics measurements. This function is an alias of the \code{getGeneralValueMetrics} function.
}

\usage{
getMustangMetrics(obj, network, station, location, channel, 
                           starttime, endtime, metricName, ...)
}

\arguments{
  \item{obj}{ an \code{IrisClient} object }
  \item{network}{ a character string with the two letter seismic network code }
  \item{station}{ a character string with the station code }
  \item{location}{ a character string with the location code, can be "" for wildcard all }
  \item{channel}{ a character string with the three letter channel code, can be "" for wildcard all }
  \item{starttime}{ a POSIXct class specifying the starttime (GMT) }
  \item{endtime}{ a POSIXct class specifying the endtime (GMT) }
  \item{metricName}{ a character string containing one or more comma separated metric names }
  \item{\dots}{optional arguments
     \code{constraint}{ a character string containing value constraints }
     \code{url}{ optional url of the MUSTANG measurements service }
  }
}

\details{

A blank location code should be specified as \code{location="--"}; Using \code{location=""} will return all location codes.

The default MUSTANG measurement service when \code{url} is not specified is:

\code{http://service.iris.edu/mustang/measurements/1/query?}

Data returned from MUSTANG are converted into an \R dataframe.

The optional \code{constraint} parameter is used to add constraints to the query as defined
in the \href{http://service.iris.edu/mustang/measurements/1}{MUSTANG measurements web service documentation}.
Any string passed in with the 
\code{constraint} parameter will be appended to the request url following an ampersand.

Error returns from the BSS will stop evaluation and generate an error message.

}

\value{
A dataframe with the following columns:

\preformatted{
~metricName~, value, additional values,  snclq, starttime, endtime, loadtime
}

The \code{loadtime} column contains the time at which this record was loaded into the database.

The dataframe rows will be sorted by metricName and increasing starttime.

}

%% \references{ }

\author{
Jonathan Callahan \email{jonathan.s.callahan@gmail.com}
}

\note{
The database was originally populated with a version of this package
that always assigned quality to be 'B'. Later versions obtained the
quality from the miniSEED packet (typically 'M').  Because of this
it is possible to have duplicate entries that only differ in the Q
part of their snclq.  To avoid double counting, when the webservice return
contains two records whose only difference is the quality code portion of
the of the snclq, only the record with the later loaddate will be used
in the dataframe.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{createBssUrl}},
\code{\link{getPsdMetrics}}
}

%% max preformatted length ---------------------------------------------------
\examples{
# Open a connection to IRIS DMC webservices (including the BSS)
iris <- new("IrisClient", debug=TRUE)

starttime <- as.POSIXct("2016-08-01", tz="GMT")
endtime <- starttime + 30*24*3600
metricName <- "orientation_check"

# Get the measurement dataframe
juneStats <- tryCatch(getMustangMetrics(iris,"IU","ANMO","","BH[12Z]",starttime,endtime,metricName),
                      error=function(e) {message(e)})
juneStats
}
\keyword{ webservices }
