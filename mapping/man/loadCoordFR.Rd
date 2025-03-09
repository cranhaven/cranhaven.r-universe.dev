\name{loadCoordFR}
\alias{loadCoordFR}


\title{Get France coordinates}

\description{
Loads and returns names, id, and coordinates for France statistical unit,  to use with \code{mapping} functions and other "map" functions that accept an \code{sf} object.
}

\usage{
loadCoordFR(unit = c("region"), year = c("2021", "2020", "2019"), 
            unit_subset = NULL, matchWith = NULL, dir = NULL, 
            use_cache = TRUE, use_internet = TRUE, crs = NULL)
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
Coordinates are download from the Github repo \url{https://github.com/mappinguniverse/geospatial} from FR folder \url{https://github.com/mappinguniverse/geospatial/tree/master/FR}.

If \code{unit} is not specified, country borders are loaded.


}
\value{
A data.frame object with column indicating names, id, and the geometry to map.

}

\examples{

\donttest{
FR_coords = loadCoordFR(unit = "region", year = "2020")
str(FR_coords)

}

}


