\name{sampleRateRespMetric}

\alias{sampleRateRespMetric}

\title{Sample rate consistency between miniSEED and metadata}

\description{
The sampleRateRespMetric() function compares the miniSEED sample rate with  
the sample rate derived from the 
high-frequency corner of the channel's amplitude response.  
}

\usage{
sampleRateRespMetric(st,
                 resp_pct=15,
	         norm_freq=NULL,
                 evalresp=NULL)
}

\arguments{
  \item{st}{a \code{Stream} object containing a seismic signal}
  \item{resp_pct}{percentage by which the miniSEED and response-derived sample rates must agree to be considered a match}
  \item{norm_freq}{the normalization frequency at which the stationXML InstrumentSensitivity or dataless Stage 0 Sensitivity is valid, optional}
  \item{evalresp}{dataframe of freq, amp, phase information matching output of \code{getEvalresp}, optional}
}

\details{

Next the function retrieves the instrument response that corresponds with the start of the miniSEED time series, 
from frequencies one decade below the \code{norm_freq} through one decade above the miniSEED sampling frequency.  
The difference of the amplitude values,normalized for frequency spacing, are then scanned to find the first steep 
rolloff. The frequency associated with the maximum difference in the rolloff is stored as the empirical Nyquist 
frequency and multiplied by two to give the empirical response-derived sample rate.  The function then compares 
this sample rate with the miniSEED sample rate to see whether both rates agree within \code{resp_pct} percent.
The default percentage of 15% reflects that while most FIR filters roll off at around 85% of the Nyquist frequency, 
there is significant variations across instruments. If norm_freq or evalresp values are not provided, the code will
retrieve values from IRIS web services.

The sampleRateMetric function calculates and returns the following metrics:

\describe{

  \item{\bold{sample_rate_resp} -- "agreement between daily miniSEED and response-derived sample rates"}{A 
  boolean measurement that returns 0 if miniSEED and Response-derived  sample rates agree within 15\%, or 
  1 if they disagree. Response-derived sample rates assume that the high-frequency amplitude rolloff is ~85\% 
  of the Nyquist frequency.}
}

}

\value{
  A list of lists is returned containing:
  \itemize{ 
    \item{\code{m1} = list of \code{sample_rate_resp} metric objects}
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
norm_freq <- meta$scalefreq
  
# Get the waveform
st <- IRISSeismic::getDataselect(iris,"IU","ANMO","00","BHZ",starttime,endtime)

# Calculate the sample rate metrics
list1 <- sampleRateRespMetric(st,resp_pct=15,norm_freq)
  }
}

\keyword{metrics}
