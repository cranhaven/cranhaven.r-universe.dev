\name{getDistaz}
\docType{methods}
\alias{getDistaz}
\alias{getDistaz,IrisClient,numeric,numeric,numeric,numeric-method}

\title{Retrieve great circle distance information from IRIS DMC}

\description{
The \code{getDistaz} method obtains great circle distance data from
the IRIS DMC \code{distaz} web service.
}

\usage{
getDistaz(obj, latitude, longitude, staLatitude, staLongitude)
}

\arguments{
  \item{obj}{ an \code{IrisClient} object }
  \item{latitude}{ latitude of seismic event }
  \item{longitude}{ longitude of seismic event }
  \item{staLatitude}{ latitude of seismic station }
  \item{staLongitude}{ longitude of seismic station }
}

\details{
The distance-azimuth service will calculate the great-circle angular distance, azimuth,
and back azimuth between two geographic coordinate pairs. Azimuth and back azimuth are measured clockwise from North.
}


\value{
A dataframe with the following columns:

\preformatted{
ellipsoid.semiMajorAxis, ellipsoid.flattening, ellipsoid.name, fromlat, fromlon, tolat, tolon,
  azimuth,backAzimuth, distance, distanceMeters
}

Where \code{fromlat} is the event latitude, \code{fromlon} is the event longitude, \code{tolat} is the station latitude, and \code{tolon}
is the station longitude. \code{azimuth}, \code{backAzimuth}, and \code{distance} are measured in degrees. \code{distanceMeters} is distance in meters.
\code{ellipsoid.semiMajorAxis}, \code{ellipsoid.flattening}, and \code{ellipsoid.name} refer to the World Geodetic System standard coordinate system version used to correct for ellipticity when converting to geocentric latitudes.

Only a single row is returned.

}

\references{
The IRIS DMC distaz webservice:

  \url{https://service.iris.edu/irisws/distaz/1/}
}

\author{
Jonathan Callahan \email{jonathan@mazamascience.com}
}

%% \note{ }

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{IrisClient-class}}
}

\keyword{webservices}
