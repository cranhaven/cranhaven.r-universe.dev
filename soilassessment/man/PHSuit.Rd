\name{PHSuit}
\alias{PHSuit}
\title{
A function for assessing pH suitability requirements for certain crops and trees
}
\description{
This function determines the suitability classes for soil pH requirements for selected agricultural crops and forest trees
}
\usage{
PHSuit(value, crop)
}
\arguments{
  \item{value}{
Input soil pH.
}
  \item{crop}{
The crop of interest for which soil pH suitability class is sought.
}
}
\details{
The input value can be map or just a numerical entry of soil pH of a saturated paste extract
}
\value{
The output is pH suitability class for the crop. The output is integer value of suitability class: 1- highly suitable; 2 - moderately suitable; 3 - marginally suitable; 4 - currently not suitable; 5 - not suitable
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
If the input value is raster map, then the output will also be a raster map of pH suitability for the crop of interest
}
\seealso{
\code{\link{suitability}}, \code{\link{ECSuit}}, \code{\link{fertilitySuit}}
}
\examples{
PHSuit(8.4,"cauliflower")
}
