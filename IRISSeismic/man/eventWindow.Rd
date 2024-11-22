\name{eventWindow}
\docType{methods}
\alias{eventWindow}
\alias{eventWindow,Trace,numeric,missing,missing-method}
\alias{eventWindow,Trace,numeric,missing,numeric-method}
\alias{eventWindow,Trace,numeric,numeric,missing-method}
\alias{eventWindow,Trace,numeric,numeric,numeric-method}

\title{Return a portion of a trace surrounding an event.}

\description{
The \code{eventWindow} method of \code{Trace} uses the picker returned by the \code{STALTA()} method
to center a window around the the event detected by the picker.
}

\usage{
eventWindow(x, picker, threshold, windowSecs)
}

\arguments{
  \item{x}{
a \code{Trace} object
  }
  \item{picker}{
a picker as returned by the \code{STALTA()} method applied to this \code{Trace}
  }
  \item{threshold}{
the threshold at which the picker is 'triggered'
  }
  \item{windowSecs}{
the size of the window in secs
  }
}

\details{
This utility function uses the trace method \code{triggerOnset()} to determine
p-wave onset followed by the \code{slice()} method to return a new \code{Trace} object of
the desired size centered \emph{near} the event onset.

When no threshold value is supplied, the default value is calculated as:

\code{threshold=quantile(picker,0.999,na.rm=TRUE)}

}

\value{
A new \code{Trace} object is returned.
}

%% \references{ }

\author{
Jonathan Callahan \email{jonathan@mazamascience.com}
}

%% \note{ }

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
\code{\link{STALTA}}, 
\code{\link{triggerOnset}}
}

\examples{
\dontrun{
# Open a connection to IRIS DMC webservices
iris <- new("IrisClient")

starttime <- as.POSIXct("2002-04-20", tz="GMT")
endtime <- as.POSIXct("2002-04-21", tz="GMT")

# Get the waveform
st <- getDataselect(iris,"US","OXF","","BHZ",starttime,endtime)

# Seismic signal in third trace
tr <- st@traces[[3]]

# Create a picker
picker <- STALTA(tr,3,30)
threshold <- quantile(picker,0.99999,na.rm=TRUE)

# 3 rows
layout(matrix(seq(3)))

# Plot trace and p-wave closeups
closeup1 <- eventWindow(tr,picker,threshold,3600)
closeup2 <- eventWindow(tr,picker,threshold,600)
plot(tr)
plot(closeup1,subsampling=1)
abline(v=length(closeup1)/2, col='red')
plot(closeup2,subsampling=1)
abline(v=length(closeup2)/2, col='red')

# Restore default layout
layout(1)
}
}

\keyword{methods}
