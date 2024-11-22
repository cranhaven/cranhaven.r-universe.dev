\name{psdStatistics}
\alias{psdStatistics}


\title{Return statistics for a set of PSDs}

\description{
The \code{psdStatistics} function calculates a variety of information associated with the incoming set of PSDs.
}

\usage{
psdStatistics(PSDs, evalresp=NULL)
}

\arguments{
  \item{PSDs}{either a list as returned by \code{psdList} or a dataframe of PSD values obtained from the BSS}
  \item{evalresp}{dataframe of freq, amp, phase information matching output of \code{getEvalresp}, optional}
}

%%\details{ }

\value{
A list of elements:

\itemize{
\item{\code{noiseMatrix}}{ -- matrix of corrected power levels; rows=PSDs, columns=frequencies}
\item{\code{pdfMatrix}}{ -- matrix of probability density values; rows=dB level, columns=frequencies}
\item{\code{freq}}{ -- vector of frequencies associated statistics vectors and with matrix columns}
\item{\code{pdfBins}}{ -- vector of power values (dB) associated with \code{pdfMatrix}} rows
\item{\code{max}}{ -- maximum power level at each frequency}
\item{\code{min}}{ -- minimum power level at each frequency}
\item{\code{mean}}{ -- mean power level at each frequency}
\item{\code{median}}{ -- median power level at each frequency}
\item{\code{mode}}{ -- mode of power level at each frequency (obtained from \code{pdfMatrix})}
\item{\code{nlnm}}{ -- low noise model power level at each frequency}
\item{\code{nhnm}}{ -- high noise model power level at each frequency}
\item{\code{pct_above}}{ -- percent of PSDs above the high noise model at each frequency}
\item{\code{pct_below}}{ -- percent of PSDS below the low noise model at each frequency}
}

A variety of plots can be generated form the information in this list.

} %% end of value

\references{
\href{http://pubs.usgs.gov/of/2005/1438/pdf/OFR-1438.pdf}{Seismic Noise Analysis System Using Power Spectral Density Probability Density Functions} (McNamara and Boaz 2005)
}

\author{
Jonathan Callahan \email{jonathan@mazamascience.com}
}

\seealso{
\code{\link{McNamaraPSD}},
\code{\link{psdList}},
\code{\link{psdPlot}}
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

# Generate Statistics
stats <- psdStatistics(psdList)

# Just for fun plot
logPeriod <- log10(1/stats$freq)
plot(logPeriod,stats$max,ylim=c(-200,-50), las=1,
     xlab="log10(period)", ylab="Power (dB)",
     main="Model 'normal background noise' area and area of seismic signal.")
points(logPeriod,stats$min)

# Overlay a polygon showing the range between the noise models
x <- c(logPeriod,rev(logPeriod),logPeriod[1])
y <- c(stats$nhnm,rev(stats$nlnm),stats$nhnm[1])
transparentBlack <- adjustcolor('black',0.4)
polygon(x,y,col=transparentBlack)

# Overlay a polygon showing the range of measured values
y <- c(stats$max,rev(stats$min),stats$max[1])
transparentBlue <- adjustcolor('blue',0.6)
polygon(x,y,col=transparentBlue)  
}
}

\keyword{spectra}
