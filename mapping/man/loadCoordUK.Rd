\name{loadCoordUK}
\alias{loadCoordUK}

\title{Get United Kingdom coordinates}

\description{
Loads and returns names, id, and coordinates for United Kingdom statistical unit,  to use with \code{mapping} functions and other "map" functions that accept an \code{sf} object.
}

\usage{
loadCoordUK(unit = c("country", "county"), 
            year = c("2020", "2019"), scale = c("500", "20"), 
            unit_subset = NULL, matchWith = NULL, dir = NULL, 
            use_cache = TRUE, use_internet = TRUE, crs = NULL)
}

\arguments{
  \item{unit}{the type of Italian statistical unit to link}
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
Coordinates are download from the Github repo \url{https://github.com/mappinguniverse/geospatial} from UK folder \url{https://github.com/mappinguniverse/geospatial/tree/master/UK}.

If \code{unit} is not specified, country borders are loaded.


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
\code{\link{loadCoordEU}}, \code{\link{loadCoordWR}}, \code{\link{loadCoordUS}}, \code{\link{loadCoordDE}}
}


\examples{

UK_coords = loadCoordUK(unit = "country", year = "2020")
str(UK_coords)


\donttest{

## Load subset

coords_uk <- loadCoordUK(unit = "county", unit_subset = "england", matchWith = "country")
coords_uk <- loadCoordUK(unit = "county", unit_subset = "hartlepool", matchWith = "county")

}

}

