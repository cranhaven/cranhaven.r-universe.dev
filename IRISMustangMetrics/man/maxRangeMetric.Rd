\name{maxRangeMetric}

\alias{maxRangeMetric}

\title{Maximum daily sample range calculated over a rolling window}

\description{
This metric calculates the difference between the largest and smallest sample 
value in a 5-minute rolling window and returns the largest value encountered 
within a 24-hour timespan.  
}

\usage{
maxRangeMetric(st,
               window=300,
               increment=150)
}

\arguments{
  \item{st}{a \code{Stream} object containing a seismic signal}
  \item{window}{number of seconds over which to evaluate the minimum and maximum sample values}
  \item{increment}{number of seconds to advance the window for each max_range calculation}
}

\details{
For a time series passed as a \code{Stream} object, this function calculates differences 
between largest and smallest amplitudes in a series of (default) 300-second windows, 
incrementing the window by (default) 150 seconds for each difference calculated.  It reports 
the largest difference as the \bold{max_range}. 
}

\value{
  The function returns a list:
  \itemize{ 
    \item{\code{m1} = list of \code{max_range} metric objects}
  }
}

\author{
Gillian Sharer \email{gillian.sharer@earthscope.org}
}

%% \note{ }

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{SingleValueMetric}}
}

\examples{
  \dontrun{
# Open a connection to IRIS DMC webservices
iris <- new("IrisClient")

starttime <- as.POSIXct("2019-08-01",tz="GMT")
endtime <- as.POSIXct("2019-08-02",tz="GMT")

# Get the waveform
st <- getDataselect(iris,"IU","ANMO","00","BHZ",starttime,endtime)

# Calculate the max_range metric
tempList <- maxRangeMetric(st)
  }
}

\keyword{metrics}
