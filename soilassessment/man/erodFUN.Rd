\name{erodFUN}
\alias{erodFUN}
\title{
A function to estimate soil erodibility factor
}
\description{
A function to determine soil erodibility factor from a choice of different erodibility models
}
\usage{
erodFUN(sand,silt,clay,OC,texture,Struct,method)
}
\arguments{
  \item{sand}{
sand proportion (percent) of the soil texture
}
  \item{silt}{
silt proportion (percent) of the soil texture
}
  \item{clay}{
clay proportion (percent) of the soil texture
}
  \item{OC}{
soil carbon content (percent)
}
  \item{texture}{
soil texture code representing the USDA soil textural class. Use classnames("texture") to insert the correct texture code
}
  \item{Struct}{
soil structure code representing the soil structure class. Use classnames("structure") to insert the correct structure code
}
  \item{method}{
method for determining soil erodibility. The following methods are included: WSmith,Yang,Renard,Bouyoucos,Denardin,Wang,Wisch1,Wisch2,Sharpley,Cheng,Auer.
}
}
\value{
soil erodibility factor ranging between 0 and 1
}
\references{
Benavidez R, Bethana J, Maxwell D, Norton K. 2018. A review of the (Revised) Universal Soil Loss Equation ((R)USLE): with a view to increasing its global applicability and improving soil loss estimates. Hydrol. Earth Syst. Sci., 22, 6059–6086

Omuto CT and Vargas R. 2009. Combining pedometrics, remote sensing and field observations for assessing soil loss in challenging drylands: a case study of northwestern Somalia. Land Degrad. Develop. 20: 101–115
}
\author{
Christian Thine Omuto
}
\seealso{
\code{\link{erosivFUN}}, \code{\link{erodibilityRisk}}, \code{\link{sloplenFUN}}
}
\examples{
library(sp)
bx=suitabinput
sand=textureinput["sand"]
silt=textureinput["silt"]
clay=textureinput["clay"]
soc=nutrindicator["soc"]
bx$permeability=permeabilityClass(bx$texture)
bx$wsmith=erodFUN(sand$sand,silt$silt,clay$clay,soc$soc,bx$texture, bx$structure,"WSmith")
bx$renard=erodFUN(sand$sand,silt$silt,clay$clay,soc$soc,bx$texture, bx$structure,"Renard")
summary(bx$renard)
spplot(bx["wsmith"])
}
