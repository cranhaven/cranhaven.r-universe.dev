\name{basicStatsMetric}

\alias{basicStatsMetric}


\title{
Min, median, mean, rms variance, max, and number of unique values of a signal
}
\description{
The basicStatsMetric() function calculates the \code{min, median, mean, max, rmsVariance} and number of unique values for
the input seismic signal.
}

\usage{
basicStatsMetric(st)
}

\arguments{
  \item{st}{ a \code{Stream} object containing a seismic signal}
}

\details{
This metric applies the \code{min, median, mean} and \code{max} methods of \code{Stream} objects
to the \code{st} parameter to calculate the following metrics:
\itemize{
  \item \code{sample_min}
  \item \code{sample_median}
  \item \code{sample_mean}
  \item \code{sample_max}
  \item \code{sample_rms}
}

It also calculates \code{length(unique(stmerged@traces[[1]]@data))}, where stmerged is the st parameter after \code{mergeTraces} is applied to it, for the following metric:
\itemize{
  \item \code{sample_unique}
}  

Any error messages generated in the process will pass through untrapped.
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
st <- getDataselect(iris,"AK","PIN","","BHZ",starttime,endtime, inclusiveEnd=FALSE)

# Calculate some metrics and show the results
metricList <- basicStatsMetric(st)
dummy <- lapply(metricList, show)
  }
}

\keyword{metrics}
