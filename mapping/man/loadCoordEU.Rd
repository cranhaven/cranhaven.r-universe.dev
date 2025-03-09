\name{loadCoordEU}
\alias{loadCoordEU}

\title{Get European coordinates}

\description{
Loads and returns names, id, and coordinates for European countries,  to use with \code{mapping} functions and other "map" functions that accept an \code{sf} object.
}
\usage{
loadCoordEU(unit = c("nuts0", "nuts1", "nuts2", "nuts3", "urau"),
            year = c("2021","2016", "2013", "2010", "2006", "2003"),
            scale = c("20", "60"), unit_subset = NULL,
            matchWith = NULL, dir = NULL,
            use_cache = TRUE, use_internet = TRUE, crs = NULL)
}

\arguments{
\item{unit}{the type of European statistical unit to link}
  \item{year}{year of the analysis}
  \item{scale}{the scale of the map}
  \item{unit_subset}{character vector of unit names to extract}
\item{matchWith}{the type of id}
\item{dir}{local directory in which shape files are stored}
  \item{use_cache}{a logical value indicating whether to use the cache}
  \item{use_internet}{a logical value indicating wheter the coordinates are downloaded from \url{https://github.com/mappinguniverse/geospatial}. If \code{FALSE} the maps downloaded during package installation will be used}
    \item{crs}{coordinate reference system. Look at \code{\link{st_crs}}}

}
\details{
Coordinates are download from the Github repo \url{https://github.com/mappinguniverse/geospatial} from EU folder \url{https://github.com/mappinguniverse/geospatial/tree/master/EU}.

If \code{unit} is not specified, borders of the European countries are loaded.


}
\value{
A data.frame object with columns indicating names, ids, iso and the geometries to map.
}

\author{
Alessio Serafini
}

\references{
\url{https://github.com/mappinguniverse/geospatial}
}

\seealso{
\code{\link{loadCoordIT}}, \code{\link{loadCoordWR}}, \code{\link{loadCoordDE}}, \code{\link{loadCoordUK}}
}


\examples{

EU_coords = loadCoordEU(unit = "nuts0")
str(EU_coords)

coords_eu_it_de <- loadCoordEU(unit = "nuts0", unit_subset = c("italy", "germany"))
str(coords_eu_it_de)


}

