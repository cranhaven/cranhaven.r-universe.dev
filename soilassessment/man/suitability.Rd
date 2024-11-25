\name{suitability}
\alias{suitability}
\title{
A function to determine soil suitability for agricultural crops
}
\description{
This function determines soil condition classes (such as suitability, fertility, etc.) given a set of indicators.
}
\usage{
suitability(df,data)
}
\arguments{
  \item{df}{
normalized pairwise decision (nxn) matrix for comparing n soil suitability (condition) factors
}
  \item{data}{
a (nxm) matrix of n suitability (condition) factors for m locations (pixels)
}
}
\value{
A vector of soil suitability (condition) class between 0 and 5.
}
\references{
FAO, 1976. A framework for land evaluation. FAO Soils Bulletin 32

Saaty TL. 1980. The Analytic Hierarchy Process. McGraw-Hill, New York
}
\author{
Christian Thine Omuto
}
\note{
It's important to normalize and assess the adequacy of the decision matrix before using this function
}
\seealso{
fertilityRating, suitabilityClass
}
\examples{
library(sp)
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
newmap$fertilityclass2=classCode(newmap$fertility,"fertility")
spplot(newmap["fertility"])
summary(newmap$fertilityclass2)
}
}
