\name{ME_ECharm}
\alias{ME_ECharm}
\title{
Mixed-effects model for harmonizing soil electrical conductivity to the equivalent conductivity of saturated paste extract
}
\description{
A function for harmonizing soil electrical conductivity to the equivalent conductivity of saturated paste extract using mixed effects approach
}
\usage{
ME_ECharm(EC, TEXCLASS, model, soilsolution)
}
\arguments{
  \item{EC}{
a vector or single value of soil electrical conductivity to be harmonized. It should have been determined in a given soil solution (e.g. 1:2, 1:2.5 or 1:5)
}
  \item{TEXCLASS}{
soil textural class of the soil whose electrical conductivity is to be harmonized. String or test entry of USDA textural classes: Cl, ClLo,Lo,LoSa,Sa,SaCl,SaClLo,SaLo,SiCl,SiClLo,SiLo,Si,CS,MS,HCL,FS. The classes can be determined from Clay, Silt, and Sand proportions using createTexturedata function
}
\item{model}{
functional model for relating EC to be harmonized and equivalent EC of saturated paste extract. Models considered are second order polynomial, sigmoid, spherical, gaussian, exponential, power, and linear functions. The default is polynomial
}
\item{soilsolution}{
– soil:water mix ratio in which electrical conductivity was measured. The function is currently working on 1:2, 1:2.5, and 1:5. The default is 1:2
}
}
\details{
EC harmonization models, which were developed using global datasets, are designed to standardize soil electrical conductivity for applications in soil salt classification
}
\value{
numeric value of equivalent EC of saturated soil paste extract
}
\references{
Omuto, C. T., Vargas, R.-R., EL Mobarak, A., Nuha, M., Viatkin, K., & Yigini, Y. (2020). Mapping of salt-affected soils – Technical manual. FAO. https://doi.org/10.4060/ca9215en

Omuto, C. T., Minasny, B., McBratney, A. B., & Biamah, E. K. (2006). Nonlinear mixed effect modelling for improved estimation of water retention and infiltration parameters. Journal of Hydrology, 330(3–4), 748–758. https://doi.org/10.1016/j.jhydrol.2006.05.006

Pinheiro, J. C., & Bates, D. M. (2000). Mixed-Effects Models in Sand S-PLUS. Springer New York. https://doi.org/10.1007/978-1-4419-0318-1
}
\author{
Christian Thine Omuto
}
\note{
The models are currently developed for soil solutions from 1:2, 1:2.5 and 1:5 soil:water mix ratios. The function only works with USDA soil textural classes. Convert other soil textural classes to USDA classes for all applications with this function.
}
\seealso{
\code{\link{ME_PHharm}}, \code{\link{ECconversion1}}, \code{\link{ECconversion2}}
}
\examples{
ndata=data.frame(EC=c(1,0.34,5.07,12.17, 2.219),TEX=c("Cl","SaCl","LoSa", "SiCl","SaClLo"))
ndata$ESa1=ME_ECharm(ndata$EC,ndata$TEX,"power","1:5")
}
