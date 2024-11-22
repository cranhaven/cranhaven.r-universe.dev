\name{upDownTimesMetric}

\alias{upDownTimesMetric}


\title{Up/down times for a channel}

\description{
The upDownTimesMetric() function determines the times at which data collection starts and stops
within a seismic \code{Stream}.
}

\usage{
upDownTimesMetric(st, min_signal, min_gap)
}

\arguments{
  \item{st}{ a \code{Stream} object containing a seismic signal}
  \item{min_signal}{ minimum duration of a \code{Trace} in seconds (default=\code{30}) }
  \item{min_gap}{ minimum gap in seconds (default=\code{60}) }
}

\details{
This function uses the output of the \code{getUpDownTimes} method of \code{Stream} objects.
}

\value{
A list with a single \code{MultipleTimeValueMetric} object is returned.
}

%% \references{ }

\author{
Jonathan Callahan \email{jonathan@mazamascience.com}
}

\note{
%%  ~~further notes~~
See the \code{seismic} package for documentation on \code{Stream} objects and the \code{getDataselect} method.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
\code{\link{getUpDownTimes}}
}

\examples{
  \dontrun{
# Open a connection to IRIS DMC webservices
iris <- new("IrisClient")

starttime <- as.POSIXct("2012-01-24", tz="GMT")
endtime <- as.POSIXct("2012-01-25", tz="GMT")

# Get the waveform
st <- getDataselect(iris,"AK","PIN","","BHZ",starttime,endtime)

# Create the upDownTimesMetric, ignoring Traces < 3 minutes and gaps of < 5 minutes
metricList <- upDownTimesMetric(st, min_signal=180, min_gap=300)
  }
}

\keyword{metrics}
