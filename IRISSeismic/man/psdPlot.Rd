\name{psdPlot}

\alias{psdPlot}


\title{Generate plots from a set of PSDs}

\description{
The \code{psdPlot} function is used to generate plots from the data in a \code{psdList}
or \code{psdDF} dataframe.
}

\usage{
psdPlot(PSDs, style='psd', evalresp=NULL, ylo=-200, yhi=-50, showNoiseModel=TRUE, 
        showMaxMin=TRUE, showMode=TRUE, showMean=FALSE, showMedian=FALSE, ...)
}

\arguments{
  \item{PSDs}{either a list as returned by \code{psdList} or a dataframe of PSD values obtained from the BSS}
  \item{style}{character identifier of plot type: \code{'psd'} plots PSD lines, \code{'pdf'} plots the \code{pdfMatrix}}
  \item{evalresp}{dataframe of freq, amp, phase information matching output of \code{getEvalresp}, optional}
  \item{ylo}{numeric setting lower limit of plot y-axis (default=\code{-200})}
  \item{yhi}{numeric setting upper limit of plot y-axis (default=\code{-50})}
  \item{showNoiseModel}{logical controlling plotting of noise model lines (default=\code{TRUE})}
  \item{showMaxMin}{logical controlling plotting of PSD max and min lines (default=\code{TRUE})}
  \item{showMode}{logical controlling plotting of PDF mode line (default=\code{TRUE})}
  \item{showMean}{logical controlling plotting of PSD mean line (default=\code{FALSE})}
  \item{showMedian}{logical controlling plotting of PSD median line (default=\code{FALSE})}
  \item{...}{arguments to be passed to plotting methods}
}

\details{
The \code{psdPlot} function creates visualizations for sets of PSDs. Plots generated with \code{style='pdf'}
mimic the plots presented in the McNamara paper.
}

%% \value{ }

\references{
\href{http://pubs.usgs.gov/of/2005/1438/pdf/OFR-1438.pdf}{Seismic Noise Analysis System Using Power Spectral Density Probability Density Functions} (McNamara and Boaz 2005)
}

\author{
Jonathan Callahan \email{jonathan@mazamascience.com}
}

%% \note{ }

\seealso{
\code{\link{McNamaraPSD}},
\code{\link{psdList}},
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

# 'psd' line plot
psdPlot(psdList,style='psd',type='l',col=adjustcolor('black',0.3))

# McNamara 'pdf' plot
psdPlot(psdList,style='pdf')
}
}

\keyword{spectra}
