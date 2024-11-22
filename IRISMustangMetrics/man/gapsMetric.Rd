\name{gapsMetric}

\alias{gapsMetric}


\title{
Gaps and overlaps in a signal
}
\description{
The gapsMetric() function calculates metrics associated with gaps and overlaps in a seismic signal,
\emph{i.e.} when \code{st} consists of more than one \code{Trace}.
}

\usage{
gapsMetric(st)
}

\arguments{
  \item{st}{ a \code{Stream} object containing a seismic signal}
}

\details{
This function uses the output of the \code{getGaps} method of \code{Stream} objects
to calculate the following metrics:
\describe{
  \item{\code{num_gaps}:}{number of gaps found in \code{st}}
  \item{\code{max_gap}:}{legnth of maximum gap (sec) found in \code{st}}
  \item{\code{num_overlaps}:}{number of overlaps found in \code{st}}
  \item{\code{max_overlap}:}{legnth of maximum overlap (sec) found in \code{st}}
  \item{\code{percent_availability}:}{percentage of total requested time for which a signal is available}
}
The \code{requestedStarttime} and \code{requestedEndtime} slots for the \code{Stream} are used to determine
gaps before the start of the first or after the end of the last \code{Trace} in the \code{Stream}.
}

\value{
A list of \code{SingleValueMetric} objects is returned.
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

%% \seealso{ }

\examples{
  \dontrun{
# Open a connection to IRIS DMC webservices
iris <- new("IrisClient")

# Get the waveform
starttime <- as.POSIXct("2012-01-24", tz="GMT")
endtime <- as.POSIXct("2012-01-25", tz="GMT")
st <- getDataselect(iris,"AK","PIN","","BHZ",starttime,endtime)

# Calculate the gaps metrics and show the results
metricList <- gapsMetric(st)
dummy <- lapply(metricList, show)
  }
}

\keyword{metrics}
