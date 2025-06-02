\name{croatia.temp}
\alias{croatia.temp}
\docType{data}
\title{
Data climatic stations in Croatia.
}
\description{
Information of 142 climatic stations in Croatia in 2008, with topographical static predictors (Digital Elevation Model, (DEM, in meters), topographically weighted distances from the coast line (DSEA, in km), topographic wetness index (TWI))
}
\usage{data(croatia.temp)}
\format{
  A data frame with 1752 observations on the following 7 variables:
  \describe{
    \item{\code{x}}{a numeric vector; x-coordinate; Spatial reference system: UTM 33N}
    \item{\code{y}}{a numeric vector; y-coordinate; Spatial reference system: UTM 33N}
    \item{\code{t}}{a numeric vector; t-coordinate (1-12 for the months from January to December)}
    \item{\code{dem}}{a numeric vector, Digital Elevation Model (DEM, in meters)}
    \item{\code{dsea}}{a numeric vector with topographically weighted distances from the coast line (DSEA, in km)}
    \item{\code{twi}}{a numeric vector with topographic wetness index}
    \item{\code{est}}{a numeric vector with seasons (1 for January, February and March, 2 for April, May and June, 3 for July, August and September and 4 for October, November and December)}
  }
}
\references{  
Hengl, T. (2009). \emph{A Practical Guide to Geostatistical Mapping}, 2nd edn, University of Amsterdam, Amsterdam.

Hengl, T., Heuvelink Gerard, B. M., Percec Tadic, M. & Pebesma, E. J. (2012).
\emph{Spatio-temporal prediction of daily temperatures using time-series of
MODIS LST images}, Theoretical and Applied Climatology 107, 1-2, 265-277.
}
\examples{
data(croatia.temp)
summary(croatia.temp)
}
\keyword{datasets}
