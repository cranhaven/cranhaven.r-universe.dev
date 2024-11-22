\name{DCOffsetTimesMetric}

\alias{DCOffsetTimesMetric}


\title{DC Offset Detection}

\description{
The DCOffsetTimesMetric() function returns times where a shift in the signal mean is detected.
}

\usage{
DCOffsetTimesMetric(st, windowSecs, incrementSecs, threshold)
}

\arguments{
  \item{st}{a \code{Stream} object containing a seismic signal}
  \item{windowSecs}{chunk size (secs) used in DCOffset calculations (default=\code{1800})}
  \item{incrementSecs}{increment (secs) for starttime of sequential chunks (default=\code{windowSecs/2})}
  \item{threshold}{threshold used in the detection metric (default=\code{0.9})}
}

\details{
Conceptually, this algorithm asserts:  If the difference in means between sequential chunks of seismic signal
is greater than the typical std dev of a chunk then this marks a DC offset shift.

%% max preformatted length ---------------------------------------------------
Details of the algorithm are as follows
\preformatted{
# Merge all traces in the time period, filling gaps with missing values
# Break up the signal into windowSecs chunks spaced incrementSecs apart
# For each chunk calculate:
#     signal mean, signal standard deviation
# Resulting mean and std dev arrays are of length 47 for 24 hours of signal
# Metric = abs(lagged difference of chunk means) / mean(chunk std devs)
# DC offset = times when Metric > threshold
}
}

\value{
A list with a single \code{MultipleTimeValueMetric} object is returned.
}

%% \references{ }

\author{
Jonathan Callahan \email{jonathan@mazamascience.com}
}

\note{
The denominator of this metric was tested with both \code{mean(chunk std devs)} and with
\code{median(chunk std devs)} to identify a "typical" value for the chunk standard deviation.
It was found that using \code{median} resulted false offset detects whenever there was
a large seismic signal in an otherwise lo-noise signal.
}

%% \seealso{ }

\examples{
\dontrun{
# Open a connection to IRIS DMC webservices
iris <- new("IrisClient")

# Get a signal with a DC offset problem
starttime <- as.POSIXct("2012-10-26",tz="GMT")
endtime <- starttime + 2*24*3600
st <- getDataselect(iris,"IU","TARA","00","BHZ",starttime,endtime)

# Calculate the metric
metricList <- DCOffsetTimesMetric(st)

# Extract values from the first element of the list
offsetTimes <- metricList[[1]]@values

# Plot the signal and mark locations where a DC offset was detected
plot(st)
abline(v=offsetTimes,col='red')
}
}

\keyword{metrics}
