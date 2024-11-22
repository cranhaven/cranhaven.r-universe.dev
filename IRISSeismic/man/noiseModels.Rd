\name{noiseModels}

\alias{noiseModels}


\title{Generate NHNM and NLNM noise models}

\description{
The \code{noiseModels} function returns the New High Noise Model and New Low Noise Model
from the Peterson paper referenced below. Values are returned for the specific frequencies specified in
the \code{freq} argument.
}

\usage{
noiseModels(freq)
}

\arguments{
  \item{freq}{a vector of frequencies at which to generate noise model values}
}

%% \details{ }

\value{
A list is returned with elements \code{nhnm} and \code{nlnm} containing the high and
low noise models, respectively.
}

\references{
\href{https://pubs.er.usgs.gov/publication/ofr93322}{Observations of Modeling and Seismic Background Noise} (Peterson 1993)
\href{https://pubs.usgs.gov/of/2005/1438/pdf/OFR-1438.pdf}{Seismic Noise Analysis System Using Power Spectral Density Probability Density Functions} (McNamara and Boaz 2005)
}

\author{
Jonathan Callahan \email{jonathan@mazamascience.com}
}

%% \note{ }

\seealso{
\code{\link{psdStatistics}},
}


\keyword{spectra}
