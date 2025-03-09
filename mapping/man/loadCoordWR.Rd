\name{loadCoordWR}
\alias{loadCoordWR}

\title{Get worldwide countries coordinates}

\description{
Loads and returns names, ids, iso, and coordinates for world countries, ready to use with \code{mapping} functions and other "map" functions that accept an \code{sf} object.
}

\usage{
loadCoordWR(unit = c("country", "nato", "ocde",
                     "continent", "region", "subregion",
                     "region_wb", "type_income", "type_economy"),
            res = c("low", "hi"), unit_subset = NULL,
            matchWith = NULL, dir = NULL, use_cache = TRUE,
            use_internet = TRUE, crs = NULL)
}

\arguments{
  \item{unit}{the type of world statistical unit}
  \item{res}{resolution}
    \item{unit_subset}{character vector of unit names to extract}
\item{matchWith}{the type of id}
\item{dir}{local directory in which shape files are stored}
  \item{use_cache}{a logical value indicating whether to use the cache}
  \item{use_internet}{a logical value indicating wheter the coordinates are downloaded from \url{https://github.com/mappinguniverse/geospatial}. If \code{FALSE} the maps downloaded during package installation will be used.}
      \item{crs}{coordinate reference system. Look at \code{\link{st_crs}}}

}

\details{
Coordinates are download from the Github repo \url{https://github.com/mappinguniverse/geospatial} from world folder \url{https://github.com/mappinguniverse/geospatial/tree/master/world}.
}

\value{
A data.frame object with column indicating names, id, iso and the geometry to map.
}

\author{
Alessio Serafini
}

\references{
\url{https://github.com/mappinguniverse/geospatial}
}

\seealso{
\code{\link{loadCoordIT}}, \code{\link{loadCoordEU}}, \code{\link{loadCoordUS}}, \code{\link{loadCoordDE}}, \code{\link{loadCoordUK}}
}

\examples{


WR_coords = loadCoordWR(res = "low")
str(WR_coords, 1)


WR_ocde = loadCoordWR(unit = "ocde",res = "low")
str(WR_ocde, 1)

WR_continent = loadCoordWR(unit = "continent",res = "low")
str(WR_continent, 1)


}

