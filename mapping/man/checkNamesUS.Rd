\name{checkNamesUS}
\alias{checkNamesUS}

\title{Check USA names}


\description{
Check the differences between the names given in input and the names, as provided by United States Census of Bureau, of the corresponding USA statistical unit.
}

\usage{
checkNamesUS(id,
             unit = c("country", "region", "division", "state",
                      "county", "district",
                      "district_county", "urban_area"),
             year = c("2018"), matchWith = c("name", "id", "number"),
             scale = c("20", "50", "500"), return_logical = FALSE,
             print = TRUE, use_internet = TRUE)
}

\arguments{
  \item{id}{character vector with names}
  \item{unit}{the type of USA statistical unit to check}
  \item{year}{year of the analysis}
  \item{matchWith}{the type of id to check if \code{unit} is set to "states"}
 \item{scale}{the scale of the map}
  \item{return_logical}{a logical value indicating whether nomatched id are returned.}
  \item{print}{a logical value indicating whether print the nomatched names}
  \item{use_internet}{a logical value indicating wheter the coordinates are downloaded from \url{https://github.com/mappinguniverse/geospatial}. If \code{FALSE} the maps downloaded during package installation will be used.}
}

\details{
The function provides a check between id names in the dataset and the USA unit. \code{unit} starts from the largest aggregate, "country", to the smallest, "district". Since unit can change over the years, the year of the data has to be provided.

The single state can be coded in different ways, with names, id or number.
}
\value{Returns a string vector with nomatched names or a boolean vector indicating whether or not the id matched.}


\seealso{
\code{\link{checkNamesIT}}, \code{\link{checkNamesEU}}, \code{\link{checkNamesWR}}
}

\examples{
data("popUS")


ck <- checkNamesUS(popUS$id, unit = "state")
}

