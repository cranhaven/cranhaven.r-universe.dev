\name{getRotation}
\docType{methods}
\alias{getRotation}
\alias{getRotation,IrisClient,character,character,character,character,POSIXct,POSIXct,character-method}

\title{Retrieve rotated seismic data from IRIS DMC}

\description{
The \code{getRotation} method makes a request of the IRIS DMC \code{rotation} 
web service and returns a list of 3 \code{Stream} objects.
}

\usage{
getRotation(obj, network, station, location, channelSet, 
              starttime, endtime, processing)
}

\arguments{
  \item{obj}{ \code{IrisClient} object}
  \item{network}{character string with the two letter seismic network code}
  \item{station}{character string with the station code}
  \item{location}{character string with the location code}
  \item{channelSet}{the first two characters of the selected source channels}
  \item{starttime}{POSIXct class specifying the starttime (GMT)}
  \item{endtime}{POSIXct class specifying the endtime (GMT)}
  \item{processing}{optional character string with processing commands }
}

\details{
The \code{rotation} web service returns a triplet of seismic Streams, rotated according
to the processing commands.

If the location argument contains an empty string to specify a 'blank' location code, a location
code of \code{"--"} will be used in the dataselect request URL.

The \code{processing} parameter can be used to specify any type of processing supported by the
\code{rotation} webs service.  This string must begin with an ampersand and be ready to be appended to the request url, e.g.
\code{processing="&components=ZRT&azimuth=23.1"}.  This gives the user complete control over the number
and order of processing commands.
(See \href{https://service.iris.edu/irisws/rotation/1/}{rotation documentation}.)
  
Error returns from the web service will stop evaluation and generate an error message.
}

\value{
A list of three \code{Stream} objects is returned.
}

\references{
%% ~put references to the literature/web site here ~
The IRIS DMC rotation web service:

  \url{https://service.iris.edu/irisws/rotation/1/}
}

\author{
Jonathan Callahan \email{jonathan@mazamascience.com}
}

%% \note{ }

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{IrisClient-class}}
}


\keyword{webservices}
