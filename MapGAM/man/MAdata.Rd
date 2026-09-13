\name{MAdata}
\alias{MAdata}
\docType{data}
\title{
Synthetic Case-Control Data for Massachusetts
}
\description{
90 cases and 910 controls with random smoking covarate values and random geolocations within Massachusetts, geocoded on a Lambert projection (in meters).  \code{\link{MAmap}} is a map of Massachusetts using the same projection.}

\usage{data(MAdata)}
\format{
  A data frame with 1000 observations on the following 6 variables.
  \describe{
    \item{\code{Case}}{0 for controls, 1 for cases.}
    \item{\code{Xcoord}}{projected X coordinate.}
    \item{\code{Ycoord}}{projected Y coordinate.}
    \item{\code{Smoking}}{0 for nonsmokers, 1 for smokers.}
    \item{\code{Mercury}}{continuous variable for mercury exposure.}
    \item{\code{Selenium}}{continuous variable for selenium exposure.}
  }
}

\details{
Lambert conformal conic projection for the State of Massachusetts, using standard parallels 41.71666667 and 42.68333333.  The latitude of origin is 41.0, the central meridian is -71.5, and the projection units are meters (False Easting: 200000 m; False Northing: 750000 m).   
}

\source{
Simulated data provided by package authors
}

\examples{
data(MAdata)
summary(MAdata)
attach(MAdata)
# map participants, cases in red and controls in black
plot(Xcoord,Ycoord,col=Case+1) 
}

\keyword{datasets}
