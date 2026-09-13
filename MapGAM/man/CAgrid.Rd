\name{CAgrid}
\alias{CAgrid}
\docType{data}
\title{
A Grid on State of California
}
\description{
An evenly-spaced grid of prediction points approximately 5km apart that extended across the latitude and longitude coordinates of participants' locations throughout California with over 7,500 points. Areas with sparse population data are clipped.}
\usage{data(CAgrid)}
\format{
  A data frame with 7579 geolocations.
  \describe{
    \item{\code{X}}{projected X coordinate.}
    \item{\code{Y}}{projected Y coordinate.}
    }
}

\source{
Dr. Veronica Vieira, University of California, Irvine
}
\examples{
data(CAgrid)
data(CAmap)
plot(CAmap)
points(CAgrid)
}
\keyword{datasets}
