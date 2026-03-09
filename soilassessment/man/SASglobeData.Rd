\name{SASglobeData}
\alias{SASglobeData}
\title{
A function to query global SAS data
}
\description{
A function to query soil data availability in the global SAS database
}
\usage{
SASglobeData(dframe,ISO,Region)
}
\arguments{
  \item{dframe}{
is a string to describe type of soil data in the SAS database.
}
  \item{ISO}{
is a string describing three digit international ISO code for a country.
}
  \item{Region}{
is a string describing the region of the world.
}
}
\details{
Options for type of soil data for querrying the database are "ecse","ec2","ec2.5","ec5","ph","phkcl","phcacl2","sand","silt","clay".Options for regions of the world in the SAS database are "Africa", "Asia", "Europe", "Eurasia", "NENA", "LAC", "N.America", and "Pacific". NENA is Near East and North Africa. LAC is Latin America and Caribbean. N. America is North America. Any of these Regions may be specified if desired. The default Region is NULL
}
\value{
The query returns a dataframe with the soil attribute querried, coordinates of sampling locations, and name of country where the samples are located
}
\references{
Batjes, N. H., Ribeiro, E. & van Oostrum, A. Standardised soil profile data to support global mapping and modelling (WoSIS snapshot 2019). Earth Syst. Sci. Data 12, 299–320 (2020).

FAO/IIASA/ISRIC/ISS-CAS/JRC. Harmonized World Soil Database (version 1.2). (FAO and IIASA, 2012).

Omuto, C. T., Vargas. R., Abdelmagin, E.A., Mohamed, N., Viatkin, K., Yusuf, Y. Mapping of salt-affected soils – Technical manual. (FAO, 2020). doi:10.4060/ca9215en

Orgiazzi, A., Ballabio, C., Panagos, P., Jones, A. & Fernández‐Ugalde, O. LUCAS Soil, the largest expandable soil dataset for Europe: a review. Eur J Soil Sci 69, 140–153 (2018).
}
\author{
Christian Thine Omuto
}
\note{
Internet connectivity is needed for the function to work.
}
\examples{
\donttest{
wrter=SASglobeData("sand","ZAF")# For South Africa profiles
plot(Longitude~Latitude, wrter)
}
}
