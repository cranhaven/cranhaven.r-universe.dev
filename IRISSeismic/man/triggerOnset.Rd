\name{triggerOnset}
\docType{methods}
\alias{triggerOnset}
\alias{triggerOnset,Trace,numeric-method}

%% NOTE:  the following lines suppress warning messages from R CMD check but clutter up the index

%%\alias{triggerOnset,Trace-method}

\title{Event onset triggering}

\description{
The \code{triggerOnset} method of \code{Trace} objects uses the numeric vector returned by
the \code{STALTA} "first break picking" method and a user selected threshold to determine the
arrival time of a seismic event.
}

\usage{
triggerOnset(x, picker, threshold, index)
}

\arguments{
  \item{x}{ a \code{Trace} object }
  \item{picker}{ results from applying the \code{STALTA} method to this trace }
  \item{threshold}{ optional numeric value of the threshold at which triggering should occur }
  \item{index}{ optional logical to return the index (rather than the time) of event onset (default=\code{FALSE}) }
}

\details{
This method simply identifies the point at which the \code{picker} first rises above the \code{threshold}.

When no \code{threshold} is supplied, an appropriate value is calculated from the picker with:

\code{threshold <- quantile(picker,0.999,na.rm=TRUE)}.
}

\value{
A single value is returned identifying the onset of the seismic event or \code{NA} if none is detected.
The returned value wil be a \code{POSIXct} time by defult or a numeric index if \code{index=TRUE}.
}

%% \references{ }

\author{
Jonathan Callahan \email{jonathan@mazamascience.com}
}

\note{
%%  ~~further notes~~
The appropriate value for the threshold will depend upon the exact STA/LTA algorithm used and the noise level in the
signal.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
\code{\link{STALTA}}
}

\examples{
\dontrun{
# Open a connection to IRIS DMC webservices
iris <- new("IrisClient")

starttime <- as.POSIXct("2010-02-27 06:00:00",tz="GMT")
endtime <- as.POSIXct("2010-02-27 09:00:00",tz="GMT")
  
# Get the waveform
st <- getDataselect(iris,"IU","ANMO","00","BHZ",starttime,endtime)
tr <- st@traces[[1]]
picker <- STALTA(tr,3,30)

# Identify the onset of the event
to <- triggerOnset(tr,picker)

plot(tr)
abline(v=to, col='red', lwd=2)
}
}

\keyword{methods}
