\name{textureSuit}
\alias{textureSuit}
\title{
A function for assessing texture suitability requirements for certain crops and trees
}
\description{
This function determines the suitability classes for texture requirements of selected agricultural crops and forest trees
}
\usage{
textureSuit(value, crop)
}
\arguments{
  \item{value}{
Input textural class code.
}
  \item{crop}{
The crop of interest for which texture suitability class is sought.
}
}
\details{
The input value can be map or just a numerical entry of textural class code. The textural class code is obtained using classCode("texture")
}
\value{
The output is texture suitability class for the crop. The output is integer value for suitability class: 1- highly suitable; 2 - moderately suitable; 3 - marginally suitable; 4 - currently not suitable; 5 - not suitable
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
If the input value is raster map, then the output will also be a raster map of texture suitability for the crop of interest
}
\seealso{
tempSuit, PHSuit, rainSuit
}
\examples{
library(sp)
textureSuit(4,"mango")
texture=suitabinput["texture"]
texture$mango=textureSuit(texture$texture,"mango")
summary(texture$mango)
spplot(texture["mango"])
}
