\name{drainageSuit}
\alias{drainageSuit}
\title{
A function for assessing drainage suitability requirements for certain crops and trees
}
\description{
This function determines the suitability classes for drainage requirements for selected agricultural crops and forest trees
}
\usage{
drainageSuit(value, crop)
}
\arguments{
  \item{value}{
Input drainage class code
}
  \item{crop}{
The crop of interest for which drainage suitability class is sought.
}
}
\details{
The input value can be a map or an integer of drainage class code. The textural class code is obtained using classCode("drainage")
}
\value{
The output is drainage suitability class for the crop. The output is an integer value for suitability class: 1- highly suitable; 2 - moderately suitable; 3 - marginally suitable; 4 - currently not suitable; 5 - not suitable
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
If the input value is raster map, then the output will also be a raster map of drainage suitability for the crop of interest
}
\seealso{
\code{\link{suitability}}, \code{\link{PHSuit}}, \code{\link{rainSuit}}
}
\examples{
drainageSuit(6,"cassava")
}
