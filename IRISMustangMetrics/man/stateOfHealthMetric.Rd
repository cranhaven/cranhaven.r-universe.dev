\name{stateOfHealthMetric}

\alias{stateOfHealthMetric}


\title{State of Health metrics}

\description{
The \code{stateOfHealthMetric} function extracts accumulated miniSEED quality flags and a measure
of timing quality associated with the incoming seismic signal.
}

\usage{
stateOfHealthMetric(st)
}

\arguments{
  \item{st}{ a \code{Stream} object containing a seismic signal}
}

\details{
The miniSEED flags and timing_qual values are described in the SEED manual
(\url{http://www.fdsn.org/seed_manual/SEEDManual_V2.4.pdf}).

Each \code{Stream} object contains "accumulators" with counts of the number of times 
each bit flag was set during the parsing of a miniSEED file.  Metrics are reported
for a subset of these flags as show in the code snippet below:

\preformatted{
  # act_flags
  calibration_signal <- st@act_flags[1]
  timing_correction <- st@act_flags[2]
  event_begin <- st@act_flags[3]
  event_end <- st@act_flags[4]
  event_in_progress <- st@act_flags[7]
  
  # io_flags
  clock_locked <- st@io_flags[6]
                   
  # dq_flags
  amplifier_saturation <- st@dq_flags[1]
  digitizer_clipping <- st@dq_flags[2]
  spikes <- st@dq_flags[3]
  glitches <- st@dq_flags[4]
  missing_padded_data <- st@dq_flags[5]
  telemetry_sync_error <- st@dq_flags[6]
  digital_filter_charging <- st@dq_flags[7]
}

An additional "timing quality" metric gives the average value for the \code{timing_qual}
value associated with each block of miniSEED data.
}

\value{
A list of \code{SingleValueMetric} objects is returned.
}

%% \references{ }

\author{
Jonathan Callahan \email{jonathan@mazamascience.com}
}

%% \note{ }

%% ~Make other sections like Warning with \section{Warning }{....} ~

%% \seealso{ }

\examples{
  \dontrun{
# Open a connection to IRIS DMC webservices
iris <- new("IrisClient")

# Get the waveform
starttime <- as.POSIXct("2012-01-24", tz="GMT")
endtime <- as.POSIXct("2012-01-25", tz="GMT")
st <- getDataselect(iris,"AK","PIN","","BHZ",starttime,endtime)

# Generate State of Health metrics and show the results
metricList <- stateOfHealthMetric(st)
dummy <- lapply(metricList, show)
  }
}

\keyword{metrics}

