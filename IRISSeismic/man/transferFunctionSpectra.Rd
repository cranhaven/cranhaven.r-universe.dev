\name{transferFunctionSpectra}

\alias{transferFunctionSpectra}


\title{function to retrieve response values to use for input to the IRISMustangMetrics::transferFunctionMetric}

\description{
The \code{transferFunctionSpectra} function returns a frequency-amplitude-phase response from the service.iris.edu/iris/ws/evalresp web service for a seismic \code{Stream} object
using sampling_rate to determine frequency limits. The IRISMustangMetrics::transferFunctionMetric expects this output as evalresp input.
}

\usage{
transferFunctionSpectra(st,sampling_rate)
}

\arguments{
  \item{st}{a \code{Stream} object}
  \item{sampling_rate}{sample rate}
}

\details{
  The \code{transferFunctionSpectra} /determines the minfreq, maxfreq, and nfreq for input to the 
  \code{getEvalresp} function based on input sample rate. Other getEvalresp inputs are units='def' (default)
   and output='fap' (frequency-amplitude-phase).  

  \code{transferFunctionSpectra} will always call \code{getEvalresp} using the service.iris.edu/irisws/evalresp 
   web service. The IRISMustangMetrics::transferFunctionMetric expects this output as evalresp input.

}

\value{
Output is a dataframe with columns named:

\preformatted{freq, amp, phase}
}

\author{
Mary Templeton \email{met@iris.washington.edu}
}

\seealso{
\code{\link{getEvalresp}},
}

\examples{
\dontrun{
# Create a new IrisClient
iris <- new("IrisClient", debug=TRUE)

# Get seismic data
starttime <- as.POSIXct("2011-05-05", tz="GMT") 
endtime <- starttime + 1*24*3600
st <- getDataselect(iris,"IU","GRFO","--","BHE",starttime,endtime)
sampling_rate <- 20

# Generate power spectral density for each hour long segment
evalresp  <- transferFunctionSpectra(st,sampling_rate)

}
}

\keyword{spectra}
