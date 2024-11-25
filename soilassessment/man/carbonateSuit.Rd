\name{carbonateSuit}
\alias{carbonateSuit}
\title{
A function for assessing calcium carbonate suitability requirements for certain crops and trees
}
\description{
This function determines the suitability classes for soil calcium carbonate requirements of selected agricultural crops and forest trees
}
\usage{
carbonateSuit(value, crop)
}
\arguments{
  \item{value}{
Input calcium carbonate content in percent
}
\item{crop}{
The crop of interest for which calcium carbonate suitability class is sought
}
}
\details{
The input value can be map or just a numerical entry of calcium carbonate in percent
}
\value{
The output is calcium carbonate suitability class for the crop. The output is an integer value for suitability class: 1- highly suitable; 2 - moderately suitable; 3 - marginally suitable; 4 - currently not suitable; 5 - not suitable
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
\seealso{
\code{\link{suitability}}, \code{\link{ESPSuit}}, \code{\link{fertilitySuit}}
}
\examples{
library(sp)
newmap=suitabinput
newmap$saffron=carbonateSuit(newmap$cac03,"saffron")
summary(newmap$saffron)
spplot(newmap,"saffron")
}
