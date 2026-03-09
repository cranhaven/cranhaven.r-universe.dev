\name{saltRating}
\alias{saltRating}
\title{
A function for classifying salt-affected soils using EC and PH only
}
\description{
This function determines classes of salt-affected soils using Electrical Conductivity and pH according to FAO or USDA salt classification schemes
}
\usage{
saltRating(ec,ph,criterion="FAO")
}
\arguments{
   \item{ec}{
Electrical Conductivity in dS/m of saturated soil paste extract or its equivalent
}
  \item{ph}{
soil reaction (pH)
}
   \item{criterion}{
The method to use for classifying salt-affected soil. Either FAO or USDA can be selected
}
}
\value{
The output is an integer value for soil salt class. The class name for any integer code is obtained from classCode function
}
\references{
FAO.2006. Guidelines for soil description. FAO. Rome

Richards, L. A. (ed.) 1954. Diagnosis and Improvement of Saline and Alkali Soils. U.S. Department Agriculture Handbook 60. U.S. Gov. Printing Office, Washington, DC.
}
\author{
Christian Thine Omuto
}
\note{
This function gives approximate classification. A better classification is achieved when indicator of sodium ions is included (e.g. ESP)
}
\seealso{
\code{\link{saltClass}}, \code{\link{saltSeverity}}, \code{\link{classCode}}
}
\examples{
library(sp)
saltRating(11.2,8.14, "USDA")

ec=suitabinput["ec"]
ph=suitabinput["ph"]
soc=nutrindicator["soc"]
clay=textureinput["clay"]
texture=suitabinput["texture"]
newmap=ec
newmap$ph=ph$ph
newmap$ECe=ECconversion1(ec$ec,texture$texture,"FAO","1:1", soc$soc,clay$clay)
newmap$salinity=saltRating(newmap$ECe,newmap$ph,"FAO")
newmap$salineclass=classCode(newmap$salinity,"saltclass")
newmap$salineclass1=as.factor(newmap$salineclass)
spplot(newmap["salineclass"], main="Soil Salinity Class")
summary(newmap$salinity)
}
