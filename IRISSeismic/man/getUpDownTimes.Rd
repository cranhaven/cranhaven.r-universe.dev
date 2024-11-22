\name{getUpDownTimes}
\docType{methods}
\alias{getUpDownTimes}
\alias{getUpDownTimes,Stream,missing,missing-method}
\alias{getUpDownTimes,Stream,numeric,numeric-method}

\title{Determine times when a channel starts/stops collecting data}

\description{
The \code{getUpDownTimes} method determines the on/off times for data collection within a \code{Stream}
and returns a list containing these times, ignoring \code{Trace}s with a duration less than \code{min_signal} as well as data dropouts that are less than \code{min_gap}.
}

\usage{
getUpDownTimes(x, min_signal, min_gap)
}

\arguments{
  \item{x}{ \code{Stream} object }
  \item{min_signal}{ minimum \code{Trace} duration in seconds (default=30)}
  \item{min_gap}{ minimum gap in seconds (default=60)}
}

\details{
A \code{Stream} object returned by \code{\link{getDataselect}} contains a list of individual
\code{Trace} objects, each of which is guaranteed to contain a continuous array of 
data in the \code{Trace@data} slot. Each \code{Trace} also contains a \code{starttime} and an \code{endtime} representing
a period of uninterrupted data collection. Data dropouts are determined by first rejecting any \code{Trace}s of duration less than \code{min_signal}.
The temporal spacing between \code{Trace}s is then analyzed, ignoring spaces shorter than \code{min_gap}.

This method first checks the SNCL id of each \code{Trace} to make sure they are identical
and throws an error if they are not.

The first element returned is always the \code{starttime} associated the first \code{Trace}.  The last element is always the
\code{endtime} associated with the last trace.  Thus, when the first element is identical to the \code{starttime} of the web services data request this does not necessarily mean that the channel was down before this.

\bold{NOTE: } Even when data are complete for the duration of the requested timespan, the last element returned may be earlier than the \code{endtime} of the web services data request by up to a second.
}

\value{
A vector of \code{POSIXct} datetimes associated with on/off transitions.
}

%% \references{ }

\author{
Jonathan Callahan \email{jonathan@mazamascience.com}
}

%% \note{ }

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
\code{\link{plotUpDownTimes}}
}

\examples{
\dontrun{
# Open a connection to IRIS DMC webservices
iris <- new("IrisClient")

starttime <- as.POSIXct("2012-01-24", tz="GMT")
endtime <- as.POSIXct("2012-01-25", tz="GMT")

# Get the waveform
st <- getDataselect(iris,"AK","PIN","","BHZ",starttime,endtime)

# Determine up/down transitions, ignoring Traces < 3 min and gaps < 5 min
upDownTimes <- getUpDownTimes(st, min_signal=180, min_gap=300)

# Or just plot them directly
plotUpDownTimes(st, min_signal=180, min_gap=300)
}
}

\keyword{methods}
