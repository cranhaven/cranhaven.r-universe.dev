\name{basicStats}
\docType{methods}
\alias{length}
\alias{max}
\alias{mean}
\alias{median}
\alias{min}
\alias{sd}
\alias{length,Trace-method}
\alias{max,Trace-method}
\alias{mean,Trace-method}
\alias{median,Trace,logical-method}
\alias{median,Trace,missing-method}
\alias{min,Trace-method}
\alias{sd,Trace,logical-method}
\alias{sd,Trace,missing-method}
\alias{length,Stream-method}
\alias{max,Stream-method}
\alias{mean,Stream-method}
\alias{median,Stream,logical-method}
\alias{median,Stream,missing-method}
\alias{min,Stream-method}
\alias{rms,Stream,logical-method}
\alias{rms,Stream,missing-method}
\alias{rmsVariance,Stream,logical-method}
\alias{rmsVariance,Stream,missing-method}
\alias{sd,Stream,logical-method}
\alias{sd,Stream,missing-method}
\alias{parallelLength}
\alias{parallelMax}
\alias{parallelMean}
\alias{parallelMedian}
\alias{parallelMin}
\alias{parallelSd}
\alias{parallelLength,Stream-method}
\alias{parallelMax,Stream,logical-method}
\alias{parallelMax,Stream,missing-method}
\alias{parallelMean,Stream,logical-method}
\alias{parallelMean,Stream,missing-method}
\alias{parallelMedian,Stream,logical-method}
\alias{parallelMedian,Stream,missing-method}
\alias{parallelMin,Stream,logical-method}
\alias{parallelMin,Stream,missing-method}
\alias{parallelRmsVariance,Stream,logical-method}
\alias{parallelRmsVariance,Stream,missing-method}
\alias{parallelSd,Stream,logical-method}
\alias{parallelSd,Stream,missing-method}


\title{Length, Max, Mean, Median, Min and Standard Deviation}

\description{
Basic statistics on the data in \code{Trace} and \code{Stream} objects.
}

\usage{
# length(x)
# max(x, ...)
mean(x, ...)
# median(x, na.rm)
# min(x, ...)
sd(x, na.rm)
parallelLength(x)
parallelMax(x, na.rm)
parallelMean(x, na.rm)
parallelMedian(x, na.rm)
parallelMin(x, na.rm)
parallelSd(x, na.rm)
}

\arguments{
  \item{x}{a \code{Trace} or \code{Stream} object }
  \item{na.rm}{ a logical specifying whether missing values should be removed }
  \item{...}{ arguments to be passed to underlying methods, e.g. the \code{mean} function:
  \itemize{
    \item{\code{na.rm} -- as above (default=\code{FALSE}) }
   }
  }
}

\details{
\bold{Trace methods}

When \code{x} is a \code{Trace} object, methods \code{length}, \code{max}, \code{mean}, \code{median},
\code{min} and \code{sd} operate on the \code{data} slot of the \code{Trace} and are 
equivalent to, e.g., \code{max(x@data, na.rm=FALSE)}.

\bold{Stream methods}

When \code{x} is a \code{Stream} object, methods \code{length}, \code{max}, \code{mean}, \code{median},
\code{min} and \code{sd} are applied to the concatenation of data from every 
\code{Trace} in the \code{Stream}, treating this as a single data series.

The \code{parallel~} versions of these methods are available only on \code{Stream} objects
and return a vector of values, one for each \code{Trace}.

By default, the \code{Stream-method} versions of these methods use \code{na.rm=FALSE} as there
should be no missing datapoints in each \code{Trace}. The \code{Trace} methods default to 
\code{na.rm=TRUE} to accommodate merged traces where gaps have been filled with \code{NA}s.
}

\value{
For the simple statistics, a single numeric value is returned or \code{NA} if the \code{Trace} or \code{Stream}
has no data.

For the \code{parallel~} versions of these methods, available on \code{Stream} objects,
a numeric vector is returned of the same length as \code{Stream@traces}.
}

%% \references{ }

\author{
Jonathan Callahan \email{jonathan@mazamascience.com}
}

\note{
See the R documentation on the respective base functions for further details.

The \code{length.Stream} method only counts the number of actual data values in the individual
\code{Traces} in the \code{Stream} object. Missing values associated with the gaps
between \code{Traces} are not counted.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%% \seealso{ }

\examples{
\dontrun{
# Open a connection to IRIS DMC webservices
iris <- new("IrisClient")

starttime <- as.POSIXct("2012-01-24", tz="GMT")
endtime <- as.POSIXct("2012-01-25", tz="GMT")

# Get the waveform
st <- getDataselect(iris,"AK","PIN","","BHZ",starttime,endtime)

# Get the first trace and generate some statistics
tr1 <- st@traces[[1]]
length(tr1)
max(tr1)
mean(tr1)
sd(tr1)
}
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{methods}

