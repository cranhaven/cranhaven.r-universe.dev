\name{NPPmodel}
\alias{NPPmodel}
\title{
A function for calculating net primary production using air temperature and mean rainfall amount
}
\description{
This is an empirical function for deriving net primary production using climatic variables (mean temperature and rainfall amounts)
}
\usage{
NPPmodel(rain,temperature,model)
}
\arguments{
  \item{rain}{
total annual rainfall amount in mm
}
  \item{temperature}{
average annual air temperature amount in degrees Celsius
}
  \item{model}{
model for calculating net primary production. Included models in the function are Miami, Schurr, and NCEAS
}
}
\details{
This function is based on empirical models for calculating annual net primary production (NPP) of dry matter
}
\value{
Net primary production (NPP) of dry matter in grams per square meter per year
}
\references{
Schuur, E. A. G. 2003. Productivity and global climate revisited: the sensitivity of tropical forest growth to precipitation. Ecology 84:1165–1170

Lieth, H. 1975. Modeling the primary productivity of the world. Pages 237–264 in H. Lieth and R. H. Whittaker, editors. Primary productivity of the biosphere. Springer-Verlag, New York, New York, USA

Del Grosso, S., Parton, W., Stohlgren, T., Zheng, D., Bachelef, D., Prince, S., Hibbard, K., Olson, R. 2008. Global potential net primary production predicted from vegetation class, precipitation, and temperature. Ecology, 89(8): 2117-2126
}
\author{
Christian Thine Omuto
}
\note{
This empirical function estimates annual NPP in g/m2/year. It is a general model for all land cover types. It may be necessary to adjust it for certain cover types or geolocations
}
\seealso{
\code{\link{carbonTurnover}}, \code{\link{RotCmoistcorrection}}
}
\examples{
NPPmodel(800,23,"miami")
NPPmodel(800,23,"schuur")
NPPmodel(800,23,"NCEAS")
}
