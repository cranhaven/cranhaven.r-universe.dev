\name{loadCoordDE}
\alias{loadCoordDE}


\title{Get Germany coordinates}

\description{
Loads and returns names, id, and coordinates for Germany statistical unit,  to use with \code{mapping} functions and other "map" functions that accept an \code{sf} object.
}

\usage{
loadCoordDE(unit = c("state", "district", "municipal", "municipality"), 
            unit_subset = NULL, matchWith = NULL, dir = NULL, 
            use_cache = TRUE, use_internet = TRUE, crs = NULL)
}

\arguments{
  \item{unit}{the type of Italian statistical unit to link}
\item{unit_subset}{character vector of unit names to extract}
\item{matchWith}{the type of id}
\item{dir}{local directory in which shape files are stored}
\item{use_cache}{a logical value indicating whether to use the cache}
\item{use_internet}{a logical value indicating wheter the coordinates are downloaded from \url{https://github.com/mappinguniverse/geospatial}. If \code{FALSE} the maps downloaded during package installation will be used}
    \item{crs}{coordinate reference system. Look at \code{\link{st_crs}}}
}

\details{
Coordinates are download from the Github repo \url{https://github.com/mappinguniverse/geospatial} from DE folder \url{https://github.com/mappinguniverse/geospatial/tree/master/DE}.

If \code{unit} is not specified, state borders are loaded.


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
\code{\link{loadCoordEU}}, \code{\link{loadCoordWR}}, \code{\link{loadCoordUS}}, \code{\link{loadCoordUK}}
}

\examples{


DE_coords = loadCoordDE(unit = "state")
str(DE_coords)


\donttest{

## Load subset

coords_de <- loadCoordDE(unit = "district", 
                         unit_subset = "bayern", 
                         matchWith = "state")

}


}

