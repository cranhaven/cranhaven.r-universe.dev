\name{readMiniseedFile}

\alias{readMiniseedFile}

\title{Convert miniSEED file to \code{Stream} object}

\description{
The \code{readMiniseedFile} function converts a raw miniSEED file into a \code{Stream}
object.
}

\usage{
readMiniseedFile(file,sensor,scale,scalefreq,scaleunits,
                 latitude,longitude,elevation,depth,azimuth,dip)
}

\arguments{
  \item{file}{character path of a miniSEED file}
  \item{sensor}{character description of the Sensor associated with this Station-Network-Channel-Location (SNCL) (see \link{Trace})}
  \item{scale}{numeric description of the InstrumentSensitivity associated with this SNCL (see \link{Trace})}
  \item{scalefreq}{numeric description of frequency at which the InstrumentSensitivity is correct, the SensitivityFrequency (see \link{Trace})}
  \item{scaleunits}{character description of the InputUnits associated with this SNCL (see \link{Trace})}
  \item{latitude}{numeric latitude associated with this SNCL (see \link{Trace})}
  \item{longitude}{numeric longitude associated with this SNCL (see \link{Trace})}
  \item{elevation}{numeric elevation associated with this SNCL (see \link{Trace})}
  \item{depth}{numeric depth associated with this SNCL (see \link{Trace})}
  \item{azimuth}{numeric channel azimuth associated with this SNCL (see \link{Trace})}
  \item{dip}{numeric channel dip associated with this SNCL (see \link{Trace})}
}

\details{
This function reads in a raw miniSEED file and converts it to a \code{Stream} object. Metadata information is optional.
}

\value{
A \code{Stream} object.}

%% \references{ }

\author{
Jonathan Callahan \email{jonathan@mazamascience.com}
}

%% \note{ }

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
  \code{\link{miniseed2Stream}}
}


