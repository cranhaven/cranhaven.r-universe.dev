\name{crossSpectrum}
\alias{crossSpectrum}

\title{Cross-Spectral Analysis}

\description{
The crossSpectrum() function is based on \R's spec.pgram() function and attempts to provide
complete results of cross-spectral FFT analysis in a programmer-friendly fashion.
}

\usage{
crossSpectrum(x, spans = NULL, kernel = NULL, taper = 0.1,
                           pad = 0, fast = TRUE,
                           demean = FALSE, detrend = TRUE,
                           na.action = stats::na.fail)
}

\arguments{
  \item{x}{multivariate time series}
  \item{spans}{vector of odd integers giving the widths of modified Daniell smoothers to be used to smooth the periodogram}
  \item{kernel}{alternatively, a kernel smoother of class "tskernel"}
  \item{taper}{specifies the proportion of data to taper. A split cosine bell taper is applied to this proportion of the data at the beginning and end of the series}
  \item{pad}{proportion of data to pad. Zeros are added to the end of the series to increase its length by the proportion pad}
  \item{fast}{logical. if TRUE, pad the series to a highly composite length}
  \item{demean}{logical. If TRUE, subtract the mean of the series}
  \item{detrend}{logical. If TRUE, remove a linear trend from the series. This will also remove the mean}
  \item{na.action}{NA action function}
}

\details{
The multivariate timeseries passed in as the first argument should be a union of two separate timeseries with the same sampling rate created in the following manner:
\preformatted{
  ts1 <- ts(data1,frequency=sampling_rate)
  ts2 <- ts(data2,frequency=sampling_rate)
  x <- ts.union(ts1,ts2)
}

The crossSpectrum() function borrows most of its code from \R's spec.pgram() function. It omits any plotting
functionality and returns a programmer-friendly dataframe of all cross-spectral components generated during
Fourier analysis for use in calculating transfer functions.

The naming of cross-spectral components is borrowed from the Octave version of MATLAB's pwelch() function.
} %% end of details

\value{
A dataframe with the following columns:

%% max preformatted length ---------------------------------------------------
\item{\code{freq}}{spectral frequencies}
\item{\code{spec1}}{'two-sided' spectral amplitudes for ts1}
\item{\code{spec2}}{'two-sided' spectral amplitudes for ts2}
\item{\code{coh}}{magnitude squared coherence between ts1 and ts2}
\item{\code{phase}}{cross-spectral phase between ts1 and ts2}
\item{\code{Pxx}}{periodogram for ts1}
\item{\code{Pyy}}{periodogram for ts2}
\item{\code{Pxy}}{cross-periodogram for ts1 and ts2}
}

\references{

\href{https://sourceforge.net/p/octave/signal/ci/default/tree/inst/pwelch.m}{Octave pwelch() source code}

\href{https://CRAN.R-project.org/package=psd/vignettes/normalization.pdf}{Normalization of Power Spectral Density estimates}
}

\author{
Jonathan Callahan \email{jonathan@mazamascience.com}
}

%%\note{}

\seealso{
\code{\link{McNamaraPSD}}
}

\examples{
\dontrun{
# Create a new IrisClient
iris <- new("IrisClient")

# Get seismic data
starttime <- as.POSIXct("2011-05-01", tz="GMT")
endtime <- starttime + 3600

st1 <- getDataselect(iris,"CI","PASC","00","BHZ",starttime,endtime)
st2 <- getDataselect(iris,"CI","PASC","10","BHZ",starttime,endtime)
tr1 <- st1@traces[[1]]
tr2 <- st2@traces[[1]]

# Both traces have a sampling rate of 40 Hz
sampling_rate <- tr1@stats@sampling_rate

ts1 <- ts(tr1@data,frequency=sampling_rate)
ts2 <- ts(tr2@data,frequency=sampling_rate)

# Calculate the cross spectrum
DF <- crossSpectrum(ts.union(ts1,ts2),spans=c(3,5,7,9))

# Calculate the transfer function
transferFunction <- DF$Pxy / DF$Pxx
transferAmp <- Mod(transferFunction)
transferPhase <- pracma::mod(Arg(transferFunction) * 180/pi,360)

# 2 rows
layout(matrix(seq(2)))

# Plot
plot(1/DF$freq,transferAmp,type='l',log='x',
     xlab="Period (sec)",
     main="Transfer Function Amplitude")

plot(1/DF$freq,transferPhase,type='l',log='x',
     xlab="Period (sec)", ylab="degrees",
     main="Transfer Function Phase")

# Restore default layout
layout(1)
}
}

\keyword{spectra}

