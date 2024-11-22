\name{noiseMatrix2PdfMatrix}

\alias{noiseMatrix2PdfMatrix}


\title{Convert matrix of PSDs to matrix of Probability Density Functions}

\description{
This function converts a \code{noiseMatrix} returned by either \code{psdList2NoiseMatrix} or \code{psdDF2NoiseMatrix}
into a matrix of Probability Density values as defined by McNamara and Boaz 2005.
}

\usage{
noiseMatrix2PdfMatrix(noiseMatrix, lo, hi, binSize)
}

\arguments{
  \item{noiseMatrix}{a \code{noiseMatrx} returned from either \code{psdList2NoiseMatrix} or \code{psdDF2NoiseMatrix}}
  \item{lo}{lowest frequency bin (power level in dB) for the PDF y-axis (default=-200)}
  \item{hi}{highest frequency bin (power level in dB) for the PDF y-axis (default=-50)}
  \item{binSize}{size in dB of each bin (default=1)}
}

\details{
The McNamara and Boaz paper describes creating histograms of the discretized power levels at each frequency
bin associated with a set of PSDs. The value in each cell of the PDF matrix is the fraction of the corrected PSDs that
have that power level at that frequency bin.

To return a PDF matrix that matches those in the McNamara paper, use the default settings.
}

\value{
A \code{matrix} is returned with one row for each power level (-250:-50 dB) and one column for each frequency bin.
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
\code{\link{psdDF2NoiseMatrix}},
\code{\link{psdList}},
\code{\link{psdPlot}},
\code{\link{psdStatistics}}
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

# Convert into corrected "noiseMatrix"
noiseMatrix <- psdList2NoiseMatrix(psdList)

# Convert into McNamara "pdfMatrix"
pdfMatrix <- noiseMatrix2PdfMatrix(noiseMatrix)

# NOTE:  Data need to be flipped and tranposed for the XY axes in the
# NOTE:  image() function to match rows and columns in our pdfMatrix
# Plot pdfMatrix
image(t(pdfMatrix[,ncol(pdfMatrix):1]),
      col=c('gray90',rainbow(9)),
      axes=FALSE)
}
}

\keyword{spectra}
