\name{gw_variable}
\alias{gw_variable}

\title{Spatial Interaction Models: gw / regional variable}
\description{Regional variables are meant to capture the possible pull effects on internal out-migration caused by conditions elsewhere in the country (Fotheringham et al., 2002; 2004). For example (see code below), the regional variable of the total population is calculated as an index that compares the total population in a zone with the total population of the surrounding zones weighted by a second power of distance. It is used to capture a pull effect produced when an origin is surrounded by very populous zones that draw migrants from the origin (Kalogirou, 2013). Nearby locations are weighted more heavily in the calculation than more distant ones, adopting the idea of the Tobler's first law of Geography. Thus, this variable could be referred to as gw (geographically weighted) variable.
}
\usage{
gw_variable(Coords, InputVariable)
}

\arguments{
  \item{Coords}{a numeric matrix or vector or dataframe of two columns giving the X,Y coordinates of the observations (data points or geometric / population weighted centroids)}
  \item{InputVariable}{a numeric vector of a variable}
}

\value{
  \item{Regional}{a single column numeric matrix of the regional variable}
}

\references{
Fotheringham, A.S., Barmby, T., Brunsdon, C., Champion, T., Charlton, M., Kalogirou, S., Tremayne, A., Rees, P., Eyre, H., Macgill, J., Stillwell, J., Bramley, G., and Hollis, J., 2002, Development of a Migration Model:  Analytical and Practical Enhancements, Office of the Deputy Prime Minister. URL: \url{https://www.academia.edu/5274441/Development_of_a_Migration_Model_Analytical_and_Practical_Enhancements}

Fotheringham, A.S., Rees, P., Champion, T., Kalogirou, S., and Tremayne, A.R., 2004, The Development of a Migration Model for England and Wales: Overview and Modelling Out-migration, Environment and Planning A, 36, pp. 1633 - 1672. doi:10.1068/a36136

Kalogirou, S. (2003) The Statistical Analysis And Modelling Of Internal Migration Flows Within England And Wales, PhD Thesis, School of Geography, Politics and Sociology, University of Newcastle upon Tyne, UK. \url{https://theses.ncl.ac.uk/jspui/handle/10443/204}

}
\author{Stamatis Kalogirou <stamatis.science@gmail.com>}

\note{This code has been tested with Cartesian coordinates for the distances to be measured in meters. In the sample dataset GR.Municipalities the projection used is the EPSG:2100 (GGRS87 / Greek Grid)}

\examples{
data(GR.Municipalities)
GrCoords<-cbind(GR.Municipalities@data$X[1:100], GR.Municipalities@data$Y[1:100])
Regional_Population <-gw_variable(GrCoords,GR.Municipalities@data$PopTot01[1:100])
}

\keyword{regional variable}
\keyword{geographically weighted variable}
