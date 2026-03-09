\name{saltSeverity}
\alias{saltSeverity}
\title{
A function to classify salt intensity in soil
}
\description{
This function classifies salt intensity in soil based on EC, pH and ESP levels
}
\usage{
saltSeverity(ec,ph,esp,criterion="FAO")
}
\arguments{
  \item{ec}{
electrical conductivity in dS/m of saturated soil paste extract or its equivalent
}
  \item{ph}{
soil reaction (pH)
}
  \item{esp}{
Exchangeable sodium percent
}
  \item{criterion}{
classification method for severity/degree of salt problems. FAO, USDA, Amrhein, and PSALT criteria are included. Default method is FAO.
}
}
\details{
This function requires input EC, pH and ESP values to process the classification. They can be maps or numerical entries. PSALT criterion uses percent salt content instead of EC.
}
\value{
Integer classes of ranging between 1-17. The names of integer codes are obtained using classCode function
}
\references{
Abrol, IP, Yadav JSP, Massoud FI. 1988. Salt-affected soils and their management. FAO Soils Bulletin 39. FAO, Rome

Amrhein C. 1996. Australian sodic soils: Distribution, properties, and management. Soil Science 161. pp412.

FAO. 2006. Guidelines for soil description. FAO, Rome

Richards LA. 1954. Diagnosis and improvements of saline and alkali soils. Agriculture Handbook No. 60. USDA, Washington
}
\author{
Christian Thine Omuto
}
\note{
The function strictly requires input EC, pH, and ESP. Percent salt content can be used in place of EC if the criterion is PSALT
}
\seealso{
\code{\link{saltClass}}, \code{\link{saltRating}}, \code{\link{classCode}}
}
\examples{
library(sp)
saltSeverity(4.5,7.8,11.6,"USDA")
ec=suitabinput["ec"]
ph=suitabinput["ph"]
soc=nutrindicator["soc"]
clay=textureinput["clay"]
texture=suitabinput["texture"]
newmap=ec
newmap$ph=ph$ph-1
newmap$ECe=ECconversion1(ec$ec*0.25,texture$texture,"FAO","1:5",soc$soc,clay$clay)
newmap$salt=saltSeverity(newmap$ECe,newmap$ph,6.84,"FAO")
newmap$salineclass=classCode(newmap$salt,"saltseverity")
spplot(newmap["salineclass"], main="Salinity Code")
}
