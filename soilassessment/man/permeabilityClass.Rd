\name{permeabilityClass}
\alias{permeabilityClass}
\title{
A function to determine permeability class
}
\description{
This function determines the soil permeability classes according to the USDA soil textural classes
}
\usage{
permeabilityClass(texture)
}
\arguments{
  \item{texture}{
is a string decribing soil textural class
}
}
\details{
Soil textural class is according to USDA textural triangle such as SiLo, Si, SaLo. The code is represented by first two letters of the class with the first letter in upper case, e.g., Si, Lo, Cl. Class codes with combination of textures have the first two letters included in the combination, e.g., SiLo, SaClLo, etc.
}
\value{
permeability class code. 1-very slow, 2-slow, 3-moderately slow, 4-moderate, 5-moderately rapid, 6-rapid, and 7-very rapid
}
\references{
O'Geen, A. T. (2013) Soil Water Dynamics. Nature Education Knowledge 4(5):9

Soil Survey Staff. Soil Taxonomy A Basic System of Soil Classification for Making and Interpreting Soil Surveys. Agricultural Handbook No. 436. U.S. Government Printing Office Washington, DC, 1999.
}
\author{
Christian Thine Omuto
}
\seealso{
\code{\link{drainageSuit}}, \code{\link{erodFUN}}
}
\examples{
library(sp)
permeabilityClass("SaLo")
\donttest{
texture=suitabinput["texture1"]
texture$permeability=ifelse(texture$texture1=="Lo",
permeabilityClass("Lo"),ifelse(texture$texture1=="SaLo",
permeabilityClass("SaLo"),permeabilityClass("SiLo")))
str(texture$permeability)
texture$Perm=classCode(texture$permeability,"permeability")
spplot(texture["Perm"])}
}
