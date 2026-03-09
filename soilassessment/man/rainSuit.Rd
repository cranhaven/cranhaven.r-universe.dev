\name{rainSuit}
\alias{rainSuit}
\title{
A function for assessing rainfall suitability requirements for certain crops and trees
}
\description{
This function determines the suitability classes for rainfall requirements of selected agricultural crops and forest trees
}
\usage{
rainSuit(value, crop)
}
\arguments{
  \item{value}{
Input rainfall amounts in mm.
}
  \item{crop}{
The crop of interest for which rainfall suitability class is sought.
}
}
\details{
The input value can be map or just numerical entry of annual rainfall amount in mm
}
\value{
The output is rainfall suitability class for the crop. The output is an integer for suitability class: 1- highly suitable; 2 - moderately suitable; 3 - marginally suitable; 4 - currently not suitable; 5 - not suitable
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
This function assumes rainfall as the source of water for crop development
}
\seealso{
\code{\link{suitability}}, \code{\link{ESPSuit}}, \code{\link{fertilitySuit}}
}
\examples{
library(sp)
rain=(suitabinput["rain"])
rain$rainmiaz=rainSuit(rain$rain,"wheat")
summary(rain$rainmiaz)
spplot(rain["rainmiaz"])
}
