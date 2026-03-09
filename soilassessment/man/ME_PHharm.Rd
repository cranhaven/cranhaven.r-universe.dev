\name{ME_PHharm}
\alias{ME_PHharm}
\title{
Mixed-effects model for harmonizing soil pH (KCl or CaCl2) to the equivalent pH (water)
}
\description{
A function for harmonizing soil pH (KCl or CaCl2) to the equivalent pH (water) using mixed effects approach
}
\usage{
ME_PHharm(ph, TEXCLASS, model, phtype)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ph}{
a vector or single value of soil ph in KCl or CaCl2 to be harmonized
}
 \item{TEXCLASS}{
soil textural class of the soil whose ph is to be harmonized. String or test entry of USDA textural classes: Cl, ClLo, Lo,LoSa,Sa,SaCl,SaClLo,SaLo,SiCl,SiClLo,SiLo,Si,CS,MS,HCL,FS. The classes can be determined from Clay, Silt, and Sand proportions using createTexturedata function
}
 \item{model}{
functional model for relating ph in KCl or CaCl to be harmonized and equivalent ph (water). Models considered are second order polynomial, sigmoid, spherical, gaussian, exponential, power, and linear functions. The default is polynomial
}
 \item{phtype}{
KCl or CaCl2 solution for ph. The default is CaCl2
}
}
\details{
ph harmonization models, which were developed using global datasets, are designed to standardize soil ph for applications in soil salt classification
}
\value{
numeric value of equivalent ph (water)
}
\author{
Christian Thine Omuto
}
\note{
The function only works with USDA soil textural classes. Convert other soil textural classes to USDA classes for all applications with this function.
}

\seealso{
\code{\link{ME_ECharm}}, \code{\link{ECconversion1}}, \code{\link{ECconversion2}}
}
\examples{
newdata=data.frame(ph=c(1.6,8.3,5.7,12.1,2.2),tex=c("Cl","SaCl","LoSa", "Si","SaClLo"))
newdata$pH2=ME_PHharm(newdata$ph,newdata$tex,"exponential","kcl")

}
