\name{ECconversion1}
\alias{ECconversion1}
\title{
A function for harmonizing electrical conductivity of a soil solution to that of the saturated paste extract
}
\description{
This function converts electrical conductivity measurements of a soil solution to that of soil paste extract. It considers the influence of texture, organic matter content, and clay content on electrical conductivity conversion. These factors and ratio of soil:water mix for the solution and conversion method must be indicated.
}
\usage{
ECconversion1(ec,texture,method,extract,oc=NULL,clay=NULL)
}
\arguments{
  \item{ec}{
measured electrical conductivity of the soil solution
}
 \item{texture}{
soil textural class according to USDA or its equivalent. Texture class is given in terms of class codes as given in classnames("texture")
}
\item{method}{
method for converting electrical conductivity of the soil:water mix to that of the soil paste extract. The methods included are FAO, sonmez, and hogg. The default is FAO
}
 \item{extract}{
ratio of soil:water in extract solution for measuring electrical conductivity was measured. Example is 1:1, 1:2, etc. The default is 1:1
}
\item{oc}{
organic matter content of the soil in percent
}
 \item{clay}{
clay content of the soil in percent
}
}
\details{
This function considers the influence of texture and soil-water solution on conversion of electrical conductivities. The functions includes FAO, sonmez, and hogg conversion models. FAO model requires information on clay content and organic carbon content.
}
\value{
equivalent electrical conductivity of saturated soil extract
}
\references{
FAO. 2006. Soil description guidelines. FAO, Rome.

Sonmez S, Buyuktas D, Asri FO. 2008. Assessment of different soil to water ratios (1:1, 1:2.5, 1:5) in soil salinity studies. Geoderma, 144: 361-369

Hogg TTJ, Henry JL. 1984. Comparison of 1:1 and 1:2 suspensions and extracts with the saturation extracts in estimating salinity in Saskatchewan. Can. J. Soil Sci. 1984, 64, 699–704

}
\author{
Christian Thine Omuto
}
\seealso{
\code{\link{ECconversion2}}, \code{\link{ECconversion3}}, \code{\link{ECconversion4}}
}
\examples{
library(sp)
library(raster)
ECconversion1(7.31,"SiCl","FAO","1:2.5",0.91,22.5)
ec=suitabinput["ec"]
soc=nutrindicator["soc"]
clay=textureinput["clay"]
texture=suitabinput["texture"]
newmap=ec
newmap$ECe=ECconversion1(ec$ec,texture$texture,"FAO","1:2.5",soc$soc,clay$clay)
spplot(newmap["ECe"], main="Equivalent ECse")
}
