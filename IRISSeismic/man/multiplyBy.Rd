\name{multiplyBy}
\docType{methods}
\alias{multiplyBy}
\alias{multiplyBy,Stream,numeric-method}
\alias{multiplyBy,Trace,numeric-method}

\title{Multiplication by a constant}

\description{
The \code{multiplyBy} methods of \code{Trace} and \code{Stream} objects return like objects
where all \code{@data} slots have been multiplied by a constant.
}

\usage{
multiplyBy(x, y)
}

\arguments{
  \item{x}{
a \code{Trace} or \code{Stream} object
  }
  \item{y}{
a numeric multiplier
  }
}

%% \details{ }

\value{
A new \code{Trace} or \code{Stream} object is returned.
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

starttime <- as.POSIXct("2011-01-24", tz="GMT")
endtime <- as.POSIXct("2011-01-25", tz="GMT")

# Get the waveform
stRaw <- getDataselect(iris,"AK","PIN","","BHZ",starttime,endtime)

# obtain an instrument sensitivity value with getChannel metadata)
c <- getChannel(iris, "AK","PIN","","BHZ",starttime, endtime)
sensitivityValue <- c$scale

# convert raw data
st <- multiplyBy(stRaw, 1/sensitivityValue)
rmsVariance(st)

# plot trace
plot(st, ylab=c$scaleunits)
}
}

\keyword{methods}
