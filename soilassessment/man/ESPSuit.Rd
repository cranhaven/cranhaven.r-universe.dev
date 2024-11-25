\name{ESPSuit}
\alias{ESPSuit}
\title{
A function for assessing Exchangeable Sodium Percent (ESP) suitability requirements for certain crops and trees
}
\description{
This function determines the suitability classes for ESP requirements of selected agricultural crops and forest trees
}
\usage{
ESPSuit(value, crop)
}
\arguments{
  \item{value}{
Input Exchangeable Sodium Percent (ESP)
}
  \item{crop}{
crop of interest for which ESP suitability class is sought
}
}
\details{
The input value can be map or just a numerical value of Exchangeable Sodium Percent (ESP)
}
\value{
The output is ESP suitability class for the crop. The output is integer value for suitability class: 1- highly suitable; 2 - moderately suitable; 3 - marginally suitable; 4 - currently not suitable; 5 - not suitable
}
\references{
Sys, C., Van Ranst, E., Debaveye, J. and Beerneaert, F.1993. Land evaluation: Part III: Crop requirements.
Development Cooperation, Belgium.

Naidu, L.G.K., Ramamurthy, V., Challa O., Hegde, R. and Krishnan, P. 2006. Manual, Soil-site Suitability Criteria for Major Crops, National Bureau of Soil Survey and Land Use Planning, ICAR, Nagpur, India

FAO Crop Suitability Requirements: http://ecocrop.fao.org/ecocrop/srv/en/home
}
\author{
Christian Thine Omuto
}
\note{
If the input value is raster map, then the output will also be a raster map of ESP suitability for the crop of interest
}
\seealso{
\code{\link{suitability}}, \code{\link{rainSuit}}, \code{\link{fertilitySuit}}
}
\examples{
ESPSuit(8.6,"broccoli")
}
