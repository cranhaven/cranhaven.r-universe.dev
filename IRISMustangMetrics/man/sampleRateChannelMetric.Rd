\name{sampleRateChannelMetric}

\alias{sampleRateChannelMetric}

\title{Sample rate consistency between miniSEED and metadata}

\description{
The sampleRateChannelMetric() function compares the miniSEED sample rate with the sample 
rate stored in the metadata channel. 
}

\usage{
sampleRateChannelMetric(st,
                 channel_pct=1,
                 chan_rate=NULL)
}

\arguments{
  \item{st}{a \code{Stream} object containing a seismic signal}
  \item{channel_pct}{percentage by which the miniSEED and channel sample rates must agree to be considered a match}
  \item{chan_rate}{metadata channel sample rate from miniSEED blockette 52, stationXML, or other metadata representation <Channel:SampleRate> element,
        optional}
}

\details{
This function retrieves the sample rate of the first trace from a \code{Stream} object and compares 
it to the metadata channel sample rate passed as \code{chan_rate} to see whether both sample rates agree within 
\code{channel_pct} percent. If chan_rate is not provided, the code will retrieve a sample rate
from IRIS web services. 

The sampleRateChannelMetric function calculates and returns the following metrics:

\describe{

  \item{\bold{sample_rate_chan} -- "agreement between daily miniSEED and metadata channel sample rates"}{A 
  boolean measurement that returns 0 if miniSEED and Channel sample rates agree within 1\%, or 1 if they 
  disagree.}

}

}

\value{
  A list of lists is returned containing:
  \itemize{ 
    \item{\code{m1} = list of \code{sample_rate_channel} metric objects}
  }
}

\author{
Mary Templeton \email{mary.templeton@earthscope.org}
}

\seealso{
\code{\link{SingleValueMetric}}
}

\examples{
  \dontrun{
# Open a connection to IRIS DMC webservices
iris <- new("IrisClient")

starttime <- as.POSIXct("2019-08-01",tz="GMT")
endtime <- as.POSIXct("2019-08-02",tz="GMT")

# Get channel-level metadata, sample rate and normalizaton frequency
meta <- IRISSeismic::getChannel(iris, "IU","ANMO","00","BHZ",starttime,endtime)
chan_rate <- meta$samplerate
  
# Get the waveform
st <- IRISSeismic::getDataselect(iris,"IU","ANMO","00","BHZ",starttime,endtime)

# Calculate the sample rate metrics
list1 <- sampleRateChannelMetric(st,channel_pct=1,chan_rate)
  }
}

\keyword{metrics}
