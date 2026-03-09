\name{classLUT}
\alias{classLUT}
\title{
A function for developing Look-up Table (LUT) for the soil condition class map
}
\description{
This function develops a Look-Up Table (LUT) for the class type map of soil condition. LUT is important map legends or maps re-classification.
}
\usage{
classLUT(fgrid,indicator)
}
\arguments{
  \item{fgrid}{
Input classified map
}
  \item{indicator}{
The soil condition indicator of interest as contained in the input map for example, "texture", "salinity", etc.
}
}
\details{
The input raster map should contain only one band for the soil indicator for clear identification of the band.
}
\value{
The output is a dataframe containing classes in the map and corresponding unique integers
}
\author{
Christian Thine Omuto
}
\seealso{
\code{\link{classCode}}, \code{\link{classnames}}
}
\examples{
textrd=suitabinput["texture"]
\donttest{
LUT=classLUT(textrd,"texture")
LUT
}
}
