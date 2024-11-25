\name{CECSuit}
\alias{CECSuit}
\title{
A function for assessing Cation Exchange Capacity (CEC) suitability requirements for certain crops and trees
}
\description{
This function determines the suitability classes for Cation Exchange Capacity (CEC) requirements of selected agricultural crops and forest trees
}
\usage{
CECSuit(value, crop)
}
\arguments{
  \item{value}{
input Cation Exchange Capacity (Cmol(+)/kg).
}
  \item{crop}{
crop of interest for which CEC suitability class is sought
}
}
\details{
input value can be a map or just a numerical entry of CEC (cmol(+)/kg)
}
\value{
The output is CEC suitability class for the crop. The output is an integer value for suitability class: 1- highly suitable; 2 - moderately suitable; 3 - marginally suitable; 4 - currently not suitable; 5 - not suitable
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
The output raster map of CEC suitability is given if the input value is raster map
}
\seealso{
\code{\link{suitability}}, \code{\link{tempSuit}}, \code{\link{rainSuit}}
}
\examples{
CECSuit(22.4,"pineaple")
}
