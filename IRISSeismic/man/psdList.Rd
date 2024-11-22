\name{psdList}

\alias{psdList}


\title{Apply McNamara PSD algorithm to a seismic signal}

\description{
The \code{psdList} function subsets a seismic \code{Stream} object into a series of
shorter segments with 50\% overlap and uses the \code{McNamaraPSD} method to return a smoothed (aka binned)
Power Spectral Density (PSD) for each segment.
}

\usage{
psdList(st)
}

\arguments{
  \item{st}{a \code{Stream} object}
}

\details{
A \code{Stream} will be subset into segments depending upon the channel identifier (@stats@channel) associated
with this seismic data.  The binning frequencies are also channel dependent as exemplified in 
this code extract where \code{Z} is the segment length in seconds:

\preformatted{
alignFreq <- 0.1
  
if (stringr::str_detect(channel,"^L")) {
  Z <- 3 * 3600
  loFreq <- 0.001
  hiFreq <- 0.5 * tr_merged@stats@sampling_rate
} else if (stringr::str_detect(channel,"^M")) {
  Z <- 2 * 3600
  loFreq <- 0.0025
  hiFreq <- 0.5 * tr_merged@stats@sampling_rate
} else {
  Z <- 3600
  loFreq <- 0.005
  hiFreq <- 0.5 * tr_merged@stats@sampling_rate
}
}

Each new segment starts half way through the previous segment. (50\% overlap)
}

\value{
A list of \code{PSD} objects is returned. Each element of the list is
an \R \code{list} object with the following elements:

\preformatted{
freq, spec, snclq, starttime, endtime
}

\bold{Note:} Individual PSDs have not had instrument correction applied.
}

\references{
\href{http://pubs.usgs.gov/of/2005/1438/pdf/OFR-1438.pdf}{Seismic Noise Analysis System Using Power Spectral Density Probability Density Functions} (McNamara and Boaz 2005)
}

\author{
Jonathan Callahan \email{jonathan@mazamascience.com}
}

%% \note{ }

\seealso{
\code{\link{McNamaraPSD}},
\code{\link{psdList2NoiseMatrix}},
\code{\link{psdPlot}},
\code{\link{psdStatistics}},
}

\examples{
\dontrun{
# Create a new IrisClient
iris <- new("IrisClient", debug=TRUE)

# Get seismic data
starttime <- as.POSIXct("2011-05-05", tz="GMT") # 2011.125
endtime <- starttime + 1*24*3600
st <- getDataselect(iris,"IU","GRFO","--","BHE",starttime,endtime)

# Generate power spectral density for each hour long segment
psdList <- psdList(st)

# Plot uncorrected PSDs
period <- 1/psdList[[1]]$freq
plot(period, psdList[[1]]$spec, log='x', type='l',
     xlab="Period (Sec)", ylab="Power (dB)",
     main="Uncorrected PSDs")
     
for (i in seq(2:length(psdList))) {
  points(period, psdList[[i]]$spec, type='l')
}
}
}

\keyword{spectra}
