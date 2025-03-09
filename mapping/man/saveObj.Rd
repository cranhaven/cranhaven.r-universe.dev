\name{saveObj}
\alias{saveObj}

\title{Save mapping obj}

\description{Save output from \code{loadCoord} function, \code{sf} objects, \code{IT}, \code{EU}, \code{WR}, and \code{US} in different format}

\usage{
saveObj(obj, name, as = c("RData", "csv", "json", "geojson", "shp"), ...)
}

\arguments{
  \item{obj}{Output from \code{loadCoord} function, \code{sf} objects, \code{IT}, \code{EU}, \code{WR}, and \code{US}}
  \item{name}{output name}
  \item{as}{format}
  \item{\dots}{further arguments}
}

\value{No return value.}

\examples{

\dontrun{

data("popIT")
it <- IT(data = popIT, unit = "provincia", year = "2019")
saveObj(it, name = "it.RData")

}

}

