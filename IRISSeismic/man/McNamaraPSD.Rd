\name{McNamaraPSD}
\alias{McNamaraPSD}

\title{Power Spectral Density}

\description{
The McNamaraPSD() function implements the spectral density algorithm specified in
the "Data Preparation and Processing" section of
\href{http://pubs.usgs.gov/of/2005/1438/pdf/OFR-1438.pdf}{Seismic Noise Analysis System Using Power Spectral Density Probability Density Functions}.
}

\usage{
McNamaraPSD(tr, loFreq=.005, hiFreq=10, alignFreq=0.1, binned=TRUE)
}

\arguments{
  \item{tr}{a \code{Trace} object}
  \item{loFreq}{optional lo end of frequency binning range}
  \item{hiFreq}{optional hi end of frequency binning range}
  \item{alignFreq}{optional alignment frequency for determining frequency bins}
  \item{binned}{logical determining whether the return spectrum is binned}
}

\details{

This PSD algorithm is designed to be used on one to three hour segments of seismic data and will return a \code{PSD} object
containing the (potentially binned) spectrum for that segment. See the \code{\link{psdList}} function for
automatic segmenting of longer \code{Stream} objects.

The McNamara PSD algorithm is similar to MATLAB's pwelch() function and has the following steps:

\enumerate{
\item{Calculate averaged spectrum}

%% max preformatted length ---------------------------------------------------
\preformatted{
# Truncate incoming segment of trace data to nearest power of 2 samples.
# Divide each truncated segment into 13 chunks with 75\% overlap. The first
# chunk begins at 0/16 and ends at 4/16.  The 13'th chunk begins at 12/16
# and ends at 16/16.  The chunks overlap like this:
#   
#   1---5---9---3---
#    2---6---0---
#     3---7---1---
#      4---8---2---
# 
# Deman, detrend and taper the chunk.
# Calculate the 'one-sided' spectrum for the chunk.
#
# Average together all 13 spectra to get an averaged spectrum.
}

\item{Create smoothed version of spectrum with binning}

When \code{binned=TRUE}, McNamara style binning is 
turned on and a smoothed spectrum is returned that contains many fewer points than the full spectrum.
When these arguments are not specified, binning is automatically turned off and the full spectrum is returned.

Frequencies for binning are generated at 1/8 octave intervals aligned to \code{alignFreq}.
The power (dB) associated with each frequency bin is calculated by averaging over an entire octave
centered on that frequency.

\bold{Note:} The spectra returned by McNamaraPSD() have not had instrument correction applied.
Use \code{\link{getEvalresp}} to get instrument correction values for specific frequencies.

\item{convert binned spectra to decibels}

} %% end of enumerate
} %% end of details

\value{
An \R \code{list} object with the following named elements:

\preformatted{
freq, spec, snclq, starttime, endtime
}

Elements \code{freq} and \code{spec} are numeric vectors while \code{snclq}, \code{starttime}
and \code{endtime} are single values.
}

\references{
\href{http://pubs.usgs.gov/of/2005/1438/pdf/OFR-1438.pdf}{Seismic Noise Analysis System Using Power Spectral Density Probability Density Functions} (McNamara and Boaz 2005)
}

\author{
Jonathan Callahan \email{jonathan@mazamascience.com}
}

\note{
During the binning process, an arithmetic mean is used to average together power levels in decibels.
This is equivalent to averaging of power levels before conversion to dB using a geometric mean.
}

\seealso{
\code{\link{McNamaraBins}},
\code{\link{psdList}}
}


\keyword{spectra}
