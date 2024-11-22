\name{STALTAMetric}

\alias{STALTAMetric}


\title{Maximum STA/LTA of a signal}

\description{
The STALTAMetric() function calculates the maximum of STA/LTA over
the incoming seismic signal.
}

\usage{
STALTAMetric(st, staSecs, ltaSecs, increment, algorithm)
}

\arguments{
  \item{st}{a \code{Stream} object containing a seismic signal}
  \item{staSecs}{length of the \bold{s}hort term averaging window in seconds (default=3)}
  \item{ltaSecs}{length of the \bold{l}ong term averaging window in seconds (default=30)}
  \item{algorithm}{algorithm to be used (default="classic_LR")}
  \item{increment}{increment used when sliding the averaging windows to the next location (default=1)}
}

\details{
Currently supported algorithms include:

\itemize{
  \item{\code{"classic_RR"}}
  \item{\code{"classic_LR"}}
  \item{\code{"EarleAndShearer_envelope"}}
}

This metric applies the \code{STALTA} method of \code{Trace} objects
to every \code{Trace} in \code{st} with the following parameter settings:

\itemize{
  \item{\code{demean=TRUE}}
  \item{\code{detrend=TRUE}}
  \item{\code{taper=0.0}}
}

The final metric value is the maximum STALTA value found in any \code{Trace}
in this \code{Stream}.

Further details are given in the documentation for \code{STALTA.Trace()}.
}

\value{
A list with a single \code{SingleValueMetric} object is returned. The metric
name is \code{max_stalta}.
}

\references{
\href{https://en.wikipedia.org/wiki/First_break_picking}{First break picking} (Wikipedia)

\href{https://www.crewes.org/Documents/ResearchReports/2014/CRR201476.pdf}{Automatic time-picking of first arrivals on large seismic datasets} (Wong 2014)

\href{http://www.fcaglp.unlp.edu.ar/~velis/papers/PickingGeop10.pdf}{Automatic first-breaks picking: New strategies and algorithms} (Sabbione and Velis 2010)
)

}

\author{
Jonathan Callahan \email{jonathan@mazamascience.com}
}

\note{
%%  ~~further notes~~
The \code{STALTA} method of \code{Trace} objects returns a numeric vector of STA/LTA values
that has the same length as the signal data.  This is a moderately time consuming operation.
By comparison, finding the maximum value of this vector of STA/LTA values is very fast.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%% \seealso{ }

\examples{
  \dontrun{
# Open a connection to IRIS DMC webservices
iris <- new("IrisClient")

# Get the waveform
starttime <- as.POSIXct("2012-02-12",tz="GMT")
endtime <- as.POSIXct("2012-02-13",tz="GMT")
st <- getDataselect(iris,"AK","GHO","","BHN",starttime,endtime)

# Calculate the STA/LTA metric and show the results
metricList <- STALTAMetric(st)
dummy <- lapply(metricList, show)
  }
}

\keyword{metrics}
