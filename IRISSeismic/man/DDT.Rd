\name{DDT}
\docType{methods}
\alias{DDT}
\alias{DDT,Trace,logical,logical,numeric-method}
\alias{DDT,Trace,missing,missing,missing-method}

\title{Apply demean, detrend, cosine taper}

\description{
The \code{DDT} method of \code{Trace} objects returns a new \code{Trace}
where data in the \code{@data} slot have been modified.  This is typically required before 
peforming any kind of spectral analysis on the seismic trace.
}

\usage{
DDT(x, demean, detrend, taper)
}

\arguments{
  \item{x}{a \code{Trace} object}
  \item{demean}{logical specifying whether to deman (default=\code{TRUE})}
  \item{detrend}{logical specifying whether to detrend (default=\code{TRUE})}
  \item{taper}{proportion of the signal to be tapered at each end (default=0.1)}
}

\details{
%%New \code{Trace} or \code{Stream} objects are created where all data values have been multiplied by \code{y}.
Use \code{taper=0} for no tapering.
}

\value{
A new \code{Trace} object is returned.
}

%%\references{ }

\author{
Jonathan Callahan \email{jonathan@mazamascience.com}
}

%% \note{ }

%% \seealso{ }

\examples{
# Open a connection to IRIS DMC webservices
iris <- new("IrisClient")

# P-wave onset for a big quake
starttime <- as.POSIXct("2010-02-27 06:30:00", tz="GMT")
endtime <- as.POSIXct("2010-02-27 07:00:00", tz="GMT")
result <- try(st <- getDataselect(iris,"IU","ANMO","00","BHZ",starttime,endtime))
if (inherits(result,"try-error")) {
  message(geterrmessage())
} else {
  tr <- st@traces[[1]]
  trClean <- DDT(tr,TRUE,TRUE,0.1)
  layout(matrix(seq(2)))
  plot(tr)
  abline(h=0,col='gray60')
  mtext("Raw",side=3,line=-2,adj=0.05,col='red')
  plot(trClean)
  abline(h=0,col='gray60')
  mtext("Demean - Detrend - Cosine Taper",line=-2,side=3,adj=0.05,col='red')
}

# Restore default layout
layout(1)
}

\keyword{methods}
