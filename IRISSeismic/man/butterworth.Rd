\name{butterworth}
\docType{methods}
\alias{butterworth}
\alias{butterworth,Trace,numeric,missing,numeric,missing-method}
\alias{butterworth,Trace,numeric,numeric,missing,missing-method}
\alias{butterworth,Trace,numeric,numeric,numeric,character-method}
\alias{butterworth,Trace,numeric,numeric,numeric,missing-method}

\title{Apply Butterworth filter}

\description{
The \code{butterworth} method of \code{Trace} objects returns a new \code{Trace}
where data in the \code{@data} slot have been modified by applying a Butterworth filter.
}

\usage{
butterworth(x, n, low, high, type)
}

\arguments{
  \item{x}{a \code{Trace} object}
  \item{n}{filter order}
  \item{low}{frequency used in low- or stop/band-pass filters}
  \item{high}{frequency used in high or stop/band-pass filters}
  \item{type}{type of filter -- \code{'low', 'high', 'pass'} or \code{'stop'}}
}

\details{
This method creates a Butterworth filter with the specified characteristics and applies
it to the Trace data.

When only \code{n} and \code{low} are specified, a high pass filter is applied.
When only \code{n} and \code{high} are specified, a low pass filter is applied.
When \code{n} and both \code{low} and \code{high} are specified, a band pass filter is applied.
To apply a band stop filter you must specify \code{n}, \code{low}, \code{high} and \code{type='stop'}
}

\value{
A new \code{Trace} object is returned.
}

%%\references{ }

\author{
Jonathan Callahan \email{jonathan@mazamascience.com}
}

%% \note{ }

\seealso{
signal::butter, signal::filter
}

\examples{
\dontrun{
# Open a connection to IRIS DMC webservices
iris <- new("IrisClient")

# Compare to the results in figure 2a of
#
# "Determination of New Zealand Ocean Bottom Seismometer Orientation
#  via Rayleigh-Wave Polarization", Stachnik et al. 
#
# http://srl.geoscienceworld.org/content/83/4/704
#
# (note: since publication, ZU.NZ19..BH1 has been renamed BH2 and ZU.NZ19..BH2 has been renamed BH1)

starttime <- as.POSIXct("2009-02-18 22:01:07",tz="GMT")
endtime <- starttime + 630
verticalLines <- starttime + seq(30,630,100)

# Get data
stZ <- getSNCL(iris,"ZU.NZ19..BHZ",starttime,endtime)
st2 <- getSNCL(iris,"ZU.NZ19..BH2",starttime,endtime)
st1 <- getSNCL(iris,"ZU.NZ19..BH1",starttime,endtime)

# Demean, Detrend, Taper
trZ <- DDT(stZ@traces[[1]],TRUE,TRUE,0.05)
tr2 <- DDT(st2@traces[[1]],TRUE,TRUE,0.05)
tr1 <- DDT(st1@traces[[1]],TRUE,TRUE,0.05)

# Bandpass filter
trZ_f <- butterworth(trZ,2,0.02,0.04,type='pass')
tr2_f <- butterworth(tr2,2,0.02,0.04,type='pass')
tr1_f <- butterworth(tr1,2,0.02,0.04,type='pass')

# 3 rows
layout(matrix(seq(3)))

# Plot
plot(trZ_f)
abline(v=verticalLines,col='gray50',lty=2)
plot(tr2_f)
abline(v=verticalLines,col='gray50',lty=2)
plot(tr1_f)
abline(v=verticalLines,col='gray50',lty=2)

# Restore default layout
layout(1)
}
}

\keyword{methods}
