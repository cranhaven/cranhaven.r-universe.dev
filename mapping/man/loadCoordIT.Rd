\name{loadCoordIT}
\alias{loadCoordIT}

\title{Get Italian coordinates}

\description{
Loads and returns names, ids, and coordinates for Italian statistical unit, ready to use with \code{mapping} functions and other "map" functions that accept an \code{sf} object.}
\usage{
loadCoordIT(unit = c("none","ripartizione", "regione", "provincia","comune"),
            year = c("2021","2020", "2019", "2018", "2017"),
            unit_subset = NULL, matchWith = NULL,
            dir = NULL, use_cache = TRUE, use_internet = TRUE, crs = NULL)
}

\arguments{
  \item{unit}{the type of Italian statistical unit to link}
\item{year}{year of the analysis}
\item{unit_subset}{character vector of unit names to extract}
\item{matchWith}{the type of id}
\item{dir}{local directory in which shape files are stored}
\item{use_cache}{a logical value indicating whether to use the cache}
\item{use_internet}{a logical value indicating wheter the coordinates are downloaded from \url{https://github.com/mappinguniverse/geospatial}. If \code{FALSE} the maps downloaded during package installation will be used}
    \item{crs}{coordinate reference system. Look at \code{\link{st_crs}}}

}

\details{
Coordinates are download from the Github repo \url{https://github.com/mappinguniverse/geospatial} from IT folder \url{https://github.com/mappinguniverse/geospatial/tree/master/IT}.

\code{unit="none"} (default) indicates that the border of Italy is returned.

}

\value{
A data.frame object with column indicating names, id, and the geometry to map.

}

\author{
Alessio Serafini
}

\references{
\url{https://github.com/mappinguniverse/geospatial}
}

\seealso{
\code{\link{loadCoordEU}}, \code{\link{loadCoordWR}}, \code{\link{loadCoordUS}}, \code{\link{loadCoordDE}}, \code{\link{loadCoordUK}}
}

\examples{

IT_coords = loadCoordIT(unit = "regione", year = "2020")
str(IT_coords)


\donttest{
## Italy

IT_coords = loadCoordIT()
str(IT_coords)


coords_it<- loadCoordIT(unit = "regione", unit_subset = c(5, 10), matchWith = "number")
str(coords_it)


}

}


