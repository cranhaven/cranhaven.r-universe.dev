\name{mergeTraces}
\docType{methods}
\alias{mergeTraces}
\alias{mergeTraces,Stream,character-method}
\alias{mergeTraces,Stream,missing-method}

\title{Merge multiple traces into a single trace}

\description{
The \code{mergeTraces} method of \code{Stream} objects returns a new \code{Stream}
where all \code{Traces} have been merged into a single \code{Trace}. Gaps between
traces are replaced with values determined by the \code{fillMethod} parameter.
}

\usage{
mergeTraces(x, fillMethod)
}

\arguments{
  \item{x}{\code{Stream} object}
  \item{fillMethod}{method to use when filling gaps between \code{Traces} (default=\code{"fillNA"})}
}

\details{
Available values for \code{fillMethod} include:
  \itemize{
    \item{\code{fillNA}}{ -- gaps are filled with \code{NA} (\R's missing value flag)}
    \item{\code{fillZero}}{ -- gaps are filled with \code{0.0}}
  }
}

\value{
A new \code{Stream} object containing a single \code{Trace} is returned.
}

%% \references{}

\author{
Jonathan Callahan \email{jonathan@mazamascience.com}
}

%% \note{ }

%% \seealso{ }

\examples{
\dontrun{
# Open a connection to IRIS DMC webservices
iris <- new("IrisClient")

starttime <- as.POSIXct("2002-04-20", tz="GMT")
endtime <- as.POSIXct("2002-04-21", tz="GMT")
st4 <- getDataselect(iris,"US","OXF","","BHZ",starttime,endtime)
stm4 <- mergeTraces(st4)

# plot merged trace
plot(stm4@traces[[1]])
mtext(paste(length(st4@traces),"traces"), side=3, line=0.5, adj=0.05, cex=1.5)      
}
}

\keyword{methods}
