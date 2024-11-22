\name{psdDF2NoiseMatrix}

\alias{psdDF2NoiseMatrix}


\title{Apply instrument correction to PSDs}

\description{
The \code{psdDF2NoiseMatrix} function uses the \code{snclq} identifier associated with the
first PSD in the dataframe to obtain instrument correction information at the specified frequencies
from the \code{\link{getEvalresp}} web service if instrumentation correction information is
not supplied as an argument. This correction is applied to every PSD in 
the dataframe and the now corrected PSD values are returned as a matrix.
}

\usage{
psdDF2NoiseMatrix(DF, evalresp=NULL)
}

\arguments{
  \item{DF}{a dataframe of PSDs obtained from the \code{getPSDMeasurements} method of \code{IrisClient}.}
  \item{evalresp}{dataframe of freq, amp, phase information matching output of \code{getEvalresp}, optional.}
}

\details{
This function is identical in behavior to \code{\link{psdList2NoiseMatrix}} except that the input object
is a dataframe of PSD values obtained from the MUSTANG Backend Storage System.
}

\value{
A \code{matrix} is returned with one row for each instrument-corrected PSD and one column for each frequency bin.
}

\references{
\href{http://pubs.usgs.gov/of/2005/1438/pdf/OFR-1438.pdf}{Seismic Noise Analysis System Using Power Spectral Density Probability Density Functions} (McNamara and Boaz 2005)
}

\author{
Jonathan Callahan \email{jonathan@mazamascience.com}
}

\note{
The incoming dataframe is checked to make sure that it represents only a single SNCL (Station-Network-Channel-Location). An error is generated if
more than one is found.  However, the \code{psdDF} is not checked to make sure that no changes to the
instrument correction happened during the time period covered by the \code{psdDF}. This occurs at an 'epoch'
boundary when an instrument is replaced.
}

\seealso{
\code{\link{McNamaraPSD}},
\code{\link{psdList}},
\code{\link{psdList2NoiseMatrix}},
\code{\link{psdPlot}},
\code{\link{psdStatistics}}
}


\keyword{spectra}
