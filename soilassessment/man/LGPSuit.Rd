\name{LGPSuit}
\alias{LGPSuit}
\title{
A function for assessing Length of Growing Period (LGP) suitability requirements for certain crops and trees
}
\description{
This function determines the suitability classes for length of growing period (LGP) requirements for selected agricultural crops and forest trees
}
\usage{
LGPSuit(value, crop)
}
\arguments{
  \item{value}{
Input length of growing period (LGP) in days.
}
  \item{crop}{
The crop of interest for which length of growing period (LGP) suitability class is sought
}
}
\details{
The input value can be map or an integer value of LGP in days
}
\value{
The output is LGP suitability class for the crop. The output is an integer for suitability class: 1- highly suitable; 2 - moderately suitable; 3 - marginally suitable; 4 - currently not suitable; 5 - not suitable
}
\references{
Sys, C., Van Ranst, E., Debaveye, J. and Beerneaert, F.1993. Land evaluation: Part III: Crop requirements.
Development Cooperation, Belgium.

Naidu, L.G.K., Ramamurthy, V., Challa O., Hegde, R. and Krishnan, P. 2006. Manual, Soil-site Suitability Criteria for Major Crops, National Bureau of Soil Survey and Land Use Planning, ICAR, Nagpur, India

FAO Crop Suitability Requirements: http://ecocrop.fao.org/ecocrop/srv/en/home
}
\author{
Christian Thine Omuto
}
\note{
If the input value is raster map, then the output will also be a raster map of LGP suitability for the crop of interest
}
\seealso{
\code{\link{suitability}}, \code{\link{PHSuit}}, \code{\link{fertilitySuit}}
}
\examples{
library(sp)
LGPSuit(138,"cotton")
newmap = data.frame(LGP = c(1:6,158,160,211),
                    lon = c(1,1,1,2,2,2,3,3,3),
                    lat = c(rep(c(0, 1.5, 3),3)))
coordinates(newmap) = ~lon+lat
gridded(newmap) = TRUE
newmap = as(newmap, "SpatialGridDataFrame")
newmap$LGPmillet=LGPSuit(newmap$LGP,"millet")
spplot(newmap["LGPmillet"], main="LGP suitability map for finger millet")
}
