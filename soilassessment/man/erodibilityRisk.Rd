\name{erodibilityRisk}
\alias{erodibilityRisk}
\title{
A function to determine soil erodibility risk
}
\description{
This function classifies soil erodibility factor into classes of risk to erosion
}
\usage{
erodibilityRisk(x)
}
\arguments{
  \item{x}{
soil erodibility factor value between 0 and 1
}
}
\details{
Erodibility factor ranges between 0 (lowest risk) to 1 (highest risk)
}
\value{
erodibility risk classes
}
\references{
Wischmeier WH, Mannering JV. 1969. Relation of Soil Properties to its Erodibility, Soil and Water Management and Conservation, 15, 131–137
Benavidez R, Bethana J, Maxwell D, Norton K. 2018. A review of the (Revised) Universal Soil Loss Equation ((R)USLE): with a view to increasing its global applicability and improving soil loss estimates. Hydrol. Earth Syst. Sci., 22, 6059–6086
}
\author{
Christian Thine Omuto
}
\seealso{
\code{\link{erosivFUN}}, \code{\link{erodFUN}}, \code{\link{sloplenFUN}}
}
\examples{
library(sp)
erodibilityRisk(0.8)
x=suitabinput
sand=textureinput["sand"]
silt=textureinput["silt"]
clay=textureinput["clay"]
soc=nutrindicator["soc"]
x$permeability=permeabilityClass(x$texture)
x$renard=erodFUN(sand$sand,silt$silt,clay$clay,soc$soc,x$texture, x$structure,"Renard")
x$erodibilityrisk=erodibilityRisk(x$renard)
x$erodib=classCode(x$renard,"erodibility")
summary(x$erodib)
spplot(x["erodib"])
}
