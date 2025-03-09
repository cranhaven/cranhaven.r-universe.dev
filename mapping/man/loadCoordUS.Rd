\name{loadCoordUS}
\alias{loadCoordUS}

\title{Get USA coordinates}

\description{
Loads and returns names, ids, and coordinates for USA, to use with \code{mapping} functions and other "map" functions that accept an \code{sf} object.
}
\usage{
loadCoordUS(unit = c("country", "region", "division", "state",
                     "county", "district", "district_county", "urban_area"),
            year = c("2018"), scale = c("20", "50", "500"),
            unit_subset = NULL, matchWith = NULL, dir = NULL,
            use_cache = TRUE, use_internet = TRUE, crs = NULL)
}

\arguments{
\item{unit}{type of USA unit to link}
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
Coordinates are downloaded from the Github repo \url{https://github.com/mappinguniverse/geospatial} from US folder \url{https://github.com/mappinguniverse/geospatial/tree/master/US}.

If \code{unit} is not specified, borders of the USA countries are loaded.
}

\value{
A data.frame object with columns indicating names, ids, and the geometry to map.
}

\author{
Alessio Serafini
}

\references{
\url{https://github.com/mappinguniverse/geospatial}
}

\seealso{
\code{\link{loadCoordIT}}, \code{\link{loadCoordWR}}, \code{\link{loadCoordDE}}, \code{\link{loadCoordUK}}, , \code{\link{loadCoordEU}}
}

\examples{

US_coords = loadCoordUS(unit = "state")
str(US_coords)


\donttest{
coords_us<- loadCoordUS(unit = "state", unit_subset = c("Florida", "California"))
}
}

