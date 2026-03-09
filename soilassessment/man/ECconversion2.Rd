\name{ECconversion2}
\alias{ECconversion2}
\title{
A function for harmonizing electrical conductivity of a soil solution to that of the saturated paste extract for all textural classes
}
\description{
This function converts electrical conductivity measurements of soil solution to that of soil paste extract. The ratio of soil:water mix for the solution and conversion method must be indicated
}
\usage{
ECconversion2(ec, method, extract)
}
\arguments{
  \item{ec}{
measured electrical conductivity of the soil solution in dS/m
}

 \item{method}{
method for converting electrical conductivity of the soil:water mix to that of the soil paste extract. The methods included are USDA, landon, kargas, ozkan, hogg, park, visconti, korsandi, shahid, klaustermeier, and he. The default is USDA
}
  \item{extract}{
ratio of soil:water in extract solution for measuring electrical conductivity was measured. Example is 1:1, 1:2, etc. Default is 1:1
}
}
\details{
This function assumes no influence of texture, clay content, etc on the conversion of electrical conductivities
}
\value{
electrical conductivity equivalent for saturated soil extract in dS/m
}
\references{
Sonmez S, Buyuktas D, Asri FO. 2008. Assessment of different soil to water ratios (1:1, 1:2.5, 1:5) in soil salinity studies. Geoderma, 144: 361-369

Kargas G, Chatzigiakoumis I, Kollias A, Spiliotis D, Massas I, Kerkides P. 2018. Soil salinity assessment using saturated paste and mass soil:water 1:1 and 1:5 ratios extracts. Water, 10:1589,  doi:10.3390/w10111589
}
\author{
Christian Thine Omuto
}
\note{
Models that work with soil solutions in 1:1 soil-water mix are: USDA, landon, kargas, ozkan,hogg, and zhang. Models for 1:2 solutions are: USDA and hogg. Models for 1:2.5 are: ozkan and shahid. landon model also works for 1:3 soil solution. Models for 1:5 are: USDA, landon, kargas, ozkan, chi, park, visconti, korsandi, klaustermeier, and he. The function only works for soil solution mix ratio handled by the respective model.
}
\seealso{
\code{\link{ECconversion1}}, \code{\link{ECconversion3}}, \code{\link{ECconversion4}}
}
\examples{
library(sp)
ECconversion2(0.75, "USDA","1:1")
newmap = suitabinput["ec"]
newmap$salinity=ECconversion2(newmap$ec,"hogg","1:1")
str(newmap$salinity)
spplot(newmap["salinity"])
}
