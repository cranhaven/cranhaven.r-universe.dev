\name{suitabilityClass}
\alias{suitabilityClass}
\title{
A function to determine suitability classes for given indicator values
}
\description{
This function determines the suitability class to which a given indicator value falls based on the crop requirement
}
\usage{
suitabilityClass(value,crop,factor)
}
\arguments{
  \item{value}{
Input indicator value.
}
  \item{crop}{
The crop of interest for which suitability is determined.
}
  \item{factor}{
The suitability factor for crop requirement. Example factors include: rain, slope, carbonate, EC, ESP, depth, ph, temperature,
}
}
\value{
The output is rainfall suitability class for the crop. The output is integer value for suitability class: 1- highly suitable; 2 - moderately suitable; 3 - marginally suitable; 4 - currently not suitable; 5 - not suitable
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
This function assumes rainfall as the source of water for crop development. The input slope value must be in degrees
}
\seealso{
suitability,slopeSuit, tempSuit
}
\examples{
library(sp)
library(raster)
suitabilityClass(20.14,"saffron","slope")
slope=suitabinput["slope"]
slope$tea=slopeSuit(slope$slope,"tea")
slope$saffron=suitabilityClass(slope$slope,"saffron","slope")
summary(slope$saffron)
spplot(slope["tea"], main="Slope suitability for tea")
spplot(slope["saffron"], main="Slope suitability for saffron")
}
