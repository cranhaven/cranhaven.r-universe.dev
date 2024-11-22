\name{slice}
\docType{methods}
\alias{slice}
\alias{slice,Trace,POSIXct,POSIXct-method}

\title{Slice a section out of a \code{Trace} or \code{Stream}}

\description{
The \code{slice} methods of \code{Trace} and \code{Stream} objects return like objects
that are subsets of the original.
}

\usage{
slice(x, starttime, endtime)
}

\arguments{
  \item{x}{ a \code{Trace} or \code{Stream} object}
  \item{starttime}{ time at which the slice should begin }
  \item{endtime}{ time at which the slice should end }
}

\details{
The returned object will always be a subset of the \code{x} argument whose time range is the
intersection of the original time range and the requested range.  When there is no intersection
or when \code{starttime > endtime} an error is generated.

All metadata associated with the returned \code{Trace} or \code{Stream} will reflect
the new object, rather than the original.
}

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

starttime <- as.POSIXct("2002-04-20", tz="GMT")
endtime <- as.POSIXct("2002-04-21", tz="GMT")

# Get the waveform
st <- getDataselect(iris,"US","OXF","","BHZ",starttime,endtime)

# This Stream object consists of 5 Traces
length(st@traces)

# Plotting the third trace shows a small quake
plot(st@traces[[3]])

# We can slice out the hour that has the quake signal
sliceStart <- as.POSIXct("2002-04-20 10:30:00", tz="GMT")
sliceEnd <- as.POSIXct("2002-04-20 11:30:00", tz="GMT")
stSlice <- slice(st, sliceStart, sliceEnd)

# Now we only have one Trace of an hour duration
length(stSlice@traces)
stSlice@traces[[1]]@stats
# And a better look at the quake signal
plot(stSlice@traces[[1]])
}
}

\keyword{methods}
