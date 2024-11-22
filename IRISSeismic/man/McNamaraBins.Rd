\name{McNamaraBins}
\alias{McNamaraBins}

\title{McNamara Binning}

\description{
The McNamaraBins() function implements the binning algorithm specified in
the "Data Preparation and Processing" section of
\href{http://pubs.usgs.gov/of/2005/1438/pdf/OFR-1438.pdf}{Seismic Noise Analysis System Using Power Spectral Density Probability Density Functions}.
}

\usage{
McNamaraBins(df, loFreq, hiFreq, alignFreq)
}

\arguments{
  \item{df}{an \R \code{dataframe} object}
  \item{loFreq}{optional lo end of frequency binning range (default=.005)}
  \item{hiFreq}{optional hi end of frequency binning range (default=10)}
  \item{alignFreq}{optional alignment frequency for determining frequency bins (default=0.1)}
}

\details{

The McNamaraBins() function accepts a dataframe with an arbitrary number of columns. At least one
of the columns must be named 'freq' and must contain frequency values. These frequencies will be used to assign all associated
values into appropriate bins according to the McNamara algorithm:

Frequencies for binning are generated at 1/8 octave intervals aligned to \code{alignFreq}.
Binned values associated with each frequency bin are calculated by averaging incoming values over an entire octave
centered on that frequency.

} %% end of details

\value{
A dataframe containing binned values is returned with the same column names as the incoming \code{df} argument.
}

\references{
\href{http://pubs.usgs.gov/of/2005/1438/pdf/OFR-1438.pdf}{Seismic Noise Analysis System Using Power Spectral Density Probability Density Functions} (McNamara and Boaz 2005)
}

\author{
Jonathan Callahan \email{jonathan@mazamascience.com}
}

%%\note{}

\seealso{
\code{\link{McNamaraPSD}}
}


\keyword{spectra}

