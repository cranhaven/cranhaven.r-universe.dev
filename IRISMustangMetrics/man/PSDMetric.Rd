\name{PSDMetric}

\alias{PSDMetric}

\title{Power Spectral Density of a signal}

\description{
The PSDMetric() function performs spectral analysis on a seismic signal
and returns 'PSD' metrics with discretized spectral components as well as other
metrics based on PSDs.
}

\usage{
PSDMetric(st,
          linLoPeriod=4/(st@traces[[1]]@stats@sampling_rate),
          linHiPeriod=100,
          evalresp=NULL,
          noCorrection=FALSE)
}

\arguments{
  \item{st}{a \code{Stream} object containing a seismic signal}
  \item{linLoPeriod}{low end of the period band use for calculating the linear dead channel metric}
  \item{linHiPeriod}{high end of the period band use for calculating the linear dead channel metric}
  \item{evalresp}{dataframe of freq, amp, phase information matching output of \code{getEvalresp}, optional}
  \item{noCorrection}{boolean (default=FALSE), TRUE=only generate list of PSDs uncorrected for instrument response;
                      FALSE=generate list of uncorrected PSDs, list of corrected PSDs, dataframe of PDF values,and PSD-derived metrics}
}

\details{
This function calculates average power spectra for a seismic signal as described in the McNamara paper.
See the \code{McNamaraPSD} method of \code{Stream} objects in the \pkg{IRISSeismic} package for details.

If optional \code{evalresp} dataframe is not supplied, the code will call \code{getEvalresp} to obtain response
information from webservices.

\emph{Uncorrected} spectral density values are returned in \code{spectrumMetricList} in units of dB.

\emph{Instrument response corrected} spectral density values are returned in \code{correctedPsdDF} in units of dB.

Probability Density Function (PDF) histogram values are returned in \code{pdfDF}. 

Other metrics calculated from the PSDs are returned in \code{svMetricList}.  These metrics are:

\describe{

  \item{\bold{pct_above_nhnm} -- "percent above New High Noise Model"}{Percentage of PSD values that
  are above the New High Noise Model for their frequency. Only frequencies less than the sample_rate/3 are
  considered to avoid instrument response effects as you approach the nyquist frequency. 
  This value is calculated over the entire time period.}

  \item{\bold{pct_below_nlnm} -- "percent below New Low Noise Model"}{Percentage of PSD values that are below the
  New Low Noise Model for their frequency. Only frequencies less than the sample_rate/3 are
  considered to avoid instrument response effects as you approach the nyquist frequency.
  This value is calculated over the entire time period.}

  \item{\bold{dead_channel_lin} -- "dead channel metric - linear fit"}{ A "dead channel" metric is calculated from
  the mean of all the PSDs generated. (Typically 47 for a 24 hour period.) Values of the PSD mean line over the band
  (linLoPeriod:linHiPeriod) are fit to a line.  The \code{dead_channel_lin} metric is the standard deviation of the
  fit residuals.  Lower numbers indicate a better fit and a higher likelihood that the mean PSD is linear -- an
  indication of a "dead channel".}

}

Note: The dead_channel_exp metric has been removed.

}

\value{
  A list of lists is returned containing:
  \itemize{ 
    \item{\code{spectrumMetricList} = list of \code{SpectrumMetric} objects}
    \item{\code{correctedPsdDF} = dataframe of starttime, endtime, frequency (Hz), power (dB) values}
    \item{\code{pdfDF} = dataframe of frequency (Hz), power (dB), hits (count) values}
    \item{\code{svMetricList} = list of \code{SingleValueMetric} objects:}
    \itemize{
      \item{\code{pct_above_nhnm}}
      \item{\code{pct_below_nlnm}}
      \item{\code{dead_channel_lin}}
    }
  }
}

\references{
\href{https://pubs.usgs.gov/of/2005/1438/pdf/OFR-1438.pdf}{Seismic Noise Analysis System Using Power Spectral Density Probability Density Functions} (McNamara and Boaz 2005)


\href{https://pubs.usgs.gov/publication/ofr93322}{Observations and Modeling of Seismic Background Noise} (Peterson 1993).
}

\author{
Jonathan Callahan \email{jonathan@mazamascience.com}
}

%% \note{ }

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{SpectrumMetric}}
\code{\link{SingleValueMetric}}
}

\examples{
  \dontrun{
# Open a connection to IRIS DMC webservices
iris <- new("IrisClient")

# NOTE:  The following trace has 1.728 million points.
# NOTE:  Downloading and calculating PSD may take a few seconds.
starttime <- as.POSIXct("2010-02-27",tz="GMT")
endtime <- as.POSIXct("2010-02-28",tz="GMT")
  
# Get the waveform
st <- getDataselect(iris,"IU","ANMO","00","BHZ",starttime,endtime)

# Calculate the PSD metric and show the SingleValueMetric results
listOfLists <- PSDMetric(st)
svMetricList <- listOfLists[['svMetricList']]

dummy <- lapply(svMetricList, show)
  }
}

\keyword{metrics}
