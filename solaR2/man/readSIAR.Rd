\name{A8_readSIAR}
\alias{readSIAR}

\title{
  Meteorological data from the SIAR network.
}

\description{Download, interpolate and transform meteorological data
  fromm the SIAR network.
}

\usage{
readSIAR(Lon = 0, Lat = 0,
         inicio = paste(year(Sys.Date())-1, '01-01', sep = '-'),
         final = paste(year(Sys.Date())-1, '12-31', sep = '-'),
         tipo = 'Mensuales', n_est = 3)
}

\arguments{
  \item{Lon}{numeric, longitude (degrees) of the location.}

  \item{Lat}{numeric, latitude (degrees) of the location.}

  \item{inicio}{character or Date, first day of the records.}

  \item{final}{character or Date, last day of the records.}

  \item{tipo}{character, tipe of the records. To choose between
    \code{Mensuales}, \code{Semanales}, \code{Diarios},
    \code{Horarios}.}

  \item{n_est}{integer, select that number of stations closest to the
    given point and then perform an IDW (Inverse Distance Weighting)
    interpolation with these data.}
}

\value{A \code{Meteo} object}

\author{
  Francisco Delgado López,
  Oscar Perpiñán Lamigueiro.
}

\seealso{
  \code{\link{readG0dm}},
  \code{\link{readBDd}}
}

\examples{
library("data.table")
\dontshow{setDTthreads(2)}
library("httr2")
library("jsonlite")

SIAR = readSIAR(Lon = -3.603, Lat = 40.033,
## Aranjuez, Comunity of Madrid, Spain
                inicio = '2023-01-01',
                final = '2023-05-01',
                tipo = 'Mensuales', n_est = 3)
SIAR

}

\keyword{constructors}
