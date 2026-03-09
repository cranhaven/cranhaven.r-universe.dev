\name{fertilitySuit}
\alias{fertilitySuit}
\title{
A function for assessing soil fertility suitability requirements for certain crops
}
\description{
This function determines the suitability classes for soil fertility requirements of selected agricultural crops
}
\usage{
fertilitySuit(value, crop)
}
\arguments{
  \item{value}{
Input soil fertility index.
}
  \item{crop}{
The crop of interest for which soil fertility suitability class is sought.
}
}
\details{
The input value can be map or just a numerical entry of soil fertility index
}
\value{
The output is fertility suitability class for the crop. The output is integer value for suitability class: 1- highly suitable; 2 - moderately suitable; 3 - marginally suitable; 4 - currently not suitable; 5 - not suitable
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
If the input value is raster map, then the output will also be a raster map of fertility suitability for the crop of interest
}
\seealso{
\code{\link{suitability}}, \code{\link{ESPSuit}}, \code{\link{fertilityRating}}
}
\examples{
library(sp)
library(FuzzyAHP)
fertilitySuit(1.56, "melon")
newmap=(nutrindicator)
newmap$carbon=fertilityRating((nutrindicator$soc),"carbon")
newmap$nitrogen=fertilityRating((nutrindicator$nitrogen),"nitrogen")
newmap$potassium=fertilityRating((nutrindicator$potassium),"potassium")
newmap$phosphorus=fertilityRating((nutrindicator$phosphorus),"phosphorus")
newmap$iron=fertilityRating((nutrindicator$iron),"iron")
newmap$zinc=fertilityRating((nutrindicator$zinc),"zinc")
newmap$manganese=fertilityRating((nutrindicator$manganese),"manganese")
newmap$copper=fertilityRating((nutrindicator$copper),"copper")
newmap$cec=fertilityRating((nutrindicator$cec),"cec")
newmap$boron=fertilityRating((nutrindicator$boron),"boron")
newmap$sulfur=fertilityRating((nutrindicator$sulfur),"sulfur")
newmap$soc=NULL
newmapT1=newmap@data
valuT=as.matrix(newmapT1)
data("nutrient")
nutriens=comparisonTable(nutrient)
\donttest{
newmapT1$fertility=suitability(nutrient, valuT)
newmap@data$fertility=newmapT1$fertility
newmap$fertilityokra=fertilitySuit(newmap$fertility,"okra")
str(newmap$fertilityokra)
spplot(newmap["fertilityokra"], main="Fertility suitability map for Okra")
}
}
