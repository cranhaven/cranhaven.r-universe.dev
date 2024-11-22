\name{correlationMetric}

\alias{correlationMetric}


\title{Correlation between channels}

\description{
The correlationMetric() function calculates the correlation between two streams of seismic data.
}

\usage{
correlationMetric(st1, st2)
}

\arguments{
  \item{st1}{a \code{Stream} object containing a seismic signal}
  \item{st2}{a \code{Stream} object containing a seismic signal}
}

\details{
The correlation returned is a value in the range [0-1]. This 'pearson r' correlation
is  a measure of the strength and direction of the linear relationship between two
variables that is defined as the (sample) covariance of the variables divided by the
product of their (sample) standard deviations.

Missing values are handled by casewise deletion with the following R code:
\preformatted{
cor(x,y,use="na.or.complete")
}
}

\value{
A list with a single \code{SingleValueMetric} object is returned.
The metric name is \code{cross_talk}.
}

%% \references{ }

\author{
Jonathan Callahan \email{jonathan@mazamascience.com}
}

\note{
Seismic streams passed to \code{correlationMetric} must have the same network and station,
must cover the same time range and must have the same sampling rate.

The metricList generated for this two-channel metric will have a SNCL code of the form:
\code{N.S.L1:L2.C1:C2.Q}.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%% \seealso{ }

\examples{
  \dontrun{
# Open a connection to IRIS DMC webservices
iris <- new("IrisClient")

# Get seismic traces
starttime <- as.POSIXct("2013-03-01", tz="GMT")
endtime <- as.POSIXct("2013-03-02",tz="GMT")
stZ <- getDataselect(iris,"IU","ANMO","00","BHZ",starttime,endtime,inclusiveEnd=FALSE)
st1 <- getDataselect(iris,"IU","ANMO","00","BH1",starttime,endtime,inclusiveEnd=FALSE)
st2 <- getDataselect(iris,"IU","ANMO","00","BH2",starttime,endtime,inclusiveEnd=FALSE)

# Calculate correlationMetric
correlationMetric(stZ,st1)[[1]]
correlationMetric(stZ,st2)[[1]]
correlationMetric(st1,st2)[[1]]
  }
}

\keyword{metrics}
