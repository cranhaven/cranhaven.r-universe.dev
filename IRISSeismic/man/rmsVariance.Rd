\name{rms}
\docType{methods}
\alias{rms}
\alias{rms,Trace,logical-method}
\alias{rms,Trace,missing-method}
\alias{rmsVariance,Trace,logical-method}
\alias{rmsVariance,Trace,missing-method}
\alias{parallelRms}
\alias{rmsVariance}
\alias{parallelRmsVariance}

%% NOTE:  the following lines suppress warning messages from R CMD check but clutter up the index

%%\alias{rmsVariance,Trace-method}
%%\alias{parallelRmsVariance,Stream-method}
%%\alias{rmsVariance,Stream-method}

\title{Root Mean Square and RMS Variance}

\description{
The \code{rms} and \code{rmsVariance} methods of \code{Trace} and \code{Stream} objects compute the 
Root Mean Square (RMS) amplitude or RMS variance of the associated data in each object.
RMS variance removes the DC level from the seismic signal so that the zero line
is consistent.
}

\usage{
rms(x, na.rm)
parallelRms(x, na.rm)
rmsVariance(x, na.rm)
parallelRmsVariance(x, na.rm)
}

\arguments{
  \item{x}{ a \code{Trace} or \code{Stream} object }
  \item{na.rm}{ a logical specifying whether missing values should be removed }
}

\details{
\bold{Trace method}

The RMS amplitude of a single \code{Trace} is calculated as:

\deqn{rms(x) = \sqrt{ \frac{\sum_1^n{(x_i)^2}}{n} } }

The RMS variance of a single \code{Trace} is calculated as:

\deqn{rmsVariance(x) = \sqrt{ \frac{\sum_1^n{(x_i - \bar{x})^2}}{n} } }

where \eqn{x} is the vector of data values and \eqn{n} is the length of that vector.

\bold{Stream methods}

For \code{Stream} objects, data from all \code{Trace}s in the stream
are first extracted and concatenated into a single numeric vector after which the
algorithm is applied.

The \code{parallel~} version of this method is only available on \code{Stream} objects
and returns a vector of values, one for each \code{Trace}.

By default, the \code{Stream} versions of these methods use \code{na.rm=FALSE} as there
should be no missing datapoints in each \code{Trace}. The \code{Trace} methods default to 
\code{na.rm=TRUE} to accommodate merged traces where gaps between traces have been
filled with \code{NA}s.
}

\value{
A single numeric value is returned or \code{NA} if the trace has no data.

A numeric vector is returned for \code{parallelRmsVariance}.
}

%% \references{ }

\author{
Jonathan Callahan \email{jonathan@mazamascience.com}
}

%% \note{ }

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
tr <- st@traces[[1]]
rmsVariance(tr)
}
}

\keyword{methods}
