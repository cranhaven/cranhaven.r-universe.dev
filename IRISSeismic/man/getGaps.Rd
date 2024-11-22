\name{getGaps}
\docType{methods}
\alias{getGaps}
\alias{getGaps,Stream,missing-method}
\alias{getGaps,Stream,numeric-method}

\title{Gap analysis}

\description{
The \code{getGaps} method calculates data dropouts that occur within the requested time
range associated with a \code{Stream}.

A \code{Stream} object returned by \code{\link{getDataselect}} contains a list of individual
\code{Trace} objects, each of which is guaranteed to contain a continuous array of 
data in each \code{Trace@data} slot. Each \code{TraceHeader} also contains a \code{starttime} and
an \code{endtime} defining a period of uninterrupted data collection.

Data dropouts are determined by examining the \code{requestedStartime} and \code{requestedEndtime}
slots associated with the \code{Stream} and the \code{starttime} and \code{endtime} slots
found in the each \code{TraceHeader}.
}

\usage{
getGaps(x, min_gap)
}

\arguments{
  \item{x}{ \code{Stream} object }
  \item{min_gap}{ minimum gap (sec) below which gaps will be ignored (default=1/sampling_rate) }
}

\details{
This method first checks the SNCL id of each \code{Trace} to make sure they are identical
and generates an error if they are not. Mismatches in the \code{sampling_rate} will also generate
an error.

The data gaps (in seconds) within a \code{Stream} are
determined and the associated \code{sampling_rate} is used to calculate the number of
missing values in each gap. The length of the \code{gaps} and \code{nsamples} vectors
in the returned list will be one more than the number of \code{Traces}
(inital gap + gaps between traces + final gap).

Gaps smaller than \code{min_gap} are set to \code{0}. Values of \code{min_gap} smaller
than \code{1/sampling_rate} will be ignored and the default value will be used instead.

Overlaps will appear as gaps with negative values.
}

\value{
A list is returned with the following elements:
  \itemize{
    \item{\code{gaps} numeric vector of data gaps within a \code{Stream}}
    \item{\code{nsamples} number of missing samples associated with each gap}
  }
}

%% \references{}

\author{
Jonathan Callahan \email{jonathan@mazamascience.com}
}
%% \note{}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%% \seealso{}

\examples{
\dontrun{
# Open a connection to IRIS DMC webservices
iris <- new("IrisClient")

starttime <- as.POSIXct("2012-01-24", tz="GMT")
endtime <- as.POSIXct("2012-01-25", tz="GMT")

# Get the waveform
st <- getDataselect(iris,"AK","PIN","","BHZ",starttime,endtime)

# Save the gap analysis in a variable
gapInfo <- getGaps(st)

# See what information is availble
names(gapInfo)

# Look at a histogram of data dropouts
hist(gapInfo$nsamples, breaks=50, 
     main="Data Gaps in AK.PIN..BHZ Jan 24, 2012",
     xlab="number of missing samples per gap")
}
}

\keyword{methods}
