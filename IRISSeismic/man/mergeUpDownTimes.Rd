\name{mergeUpDownTimes}
\docType{methods}
\alias{mergeUpDownTimes}
\alias{mergeUpDownTimes,NULL,POSIXct,logical-method}
\alias{mergeUpDownTimes,NULL,POSIXct,missing-method}
\alias{mergeUpDownTimes,POSIXct,NULL,logical-method}
\alias{mergeUpDownTimes,POSIXct,NULL,missing-method}
\alias{mergeUpDownTimes,POSIXct,POSIXct,logical-method}
\alias{mergeUpDownTimes,POSIXct,POSIXct,missing-method}

\title{Determine overlaps in two sets of upDownTimes.}

\description{
The \code{mergeUpDownTimes} function determines the overlaps in two sets of times representing up/down (on/off) periods for a single
or a set of channels.  This function can be used to determine overall station up/down periods.
}

\usage{
mergeUpDownTimes(udt1, udt2, bothOn)
}

\arguments{
  \item{udt1}{vector of \code{POSIXct} times representing up/down transitions}
  \item{udt2}{vector of \code{POSIXct} times representing up/down transitions}
  \item{bothOn}{logical specifying whether overlaps are determined with \code{AND} or \code{OR}
                 (default=FALSE: \code{udt1 OR udt2})}
}

\details{
When \code{bothOn=FALSE}, the default, this function returns the times of transitions from "either to neither" and back.
When \code{bothOn=TRUE}, this function returns the times of transitions from "both to either" and back.

If an empty vector is passed in for \code{udt1} or \code{udt2} then the other vector is returned unchanged.  This can be useful
when merging the upDownTimes for multiple channels.  See the example below.
}

\value{
A vector of \code{POSIXct} datetimes associated with on/off transitions.
}

%% \references{ }

\author{
Jonathan Callahan \email{jonathan@mazamascience.com}
}

\note{
The vector of times in \code{udt1} and \code{udt2} has no information on the values of \code{min_signal} or \code{min_gap} that
were used to generate the timeseries.  It is up to the user to make sure that the incoming vectors are appropriate for comparison.
See \code{\link{getUpDownTimes}}.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
\code{\link{getUpDownTimes}},
\code{\link{plotUpDownTimes}}
}

\examples{
\dontrun{
# Open a connection to IRIS DMC webservices
iris <- new("IrisClient")

# Three Streams, each with different upDownTimes
starttime <- as.POSIXct("2012-07-01", tz="GMT")
endtime <- as.POSIXct("2012-07-02", tz="GMT")
stE <- getDataselect(iris,"IU","XMAS","10","BHE",starttime,endtime)
stN <- getDataselect(iris,"IU","XMAS","10","BHN",starttime,endtime)
stZ <- getDataselect(iris,"IU","XMAS","10","BHZ",starttime,endtime)
udtE <- getUpDownTimes(stE)
udtN <- getUpDownTimes(stN)
udtZ <- getUpDownTimes(stZ)

udtAll <- c()
udtAny <- c()
for (udt in list(udtE, udtN, udtZ)) {
  udtAll <- mergeUpDownTimes(udtAll,udt,bothOn=TRUE)
  udtAny <- mergeUpDownTimes(udtAny,udt,bothOn=FALSE)
}

# 5 rows
layout(matrix(seq(5)))

# Plot the results
par(mar=c(3,4,3,2)) # adjust margins
plotUpDownTimes(udtE); title("BHE")
plotUpDownTimes(udtN); title("BHN")
plotUpDownTimes(udtZ); title("BHZ")
plotUpDownTimes(udtAll); title("ALL channels up")
plotUpDownTimes(udtAny); title("ANY channel up")

# Restore default layout
layout(1)
}
}

