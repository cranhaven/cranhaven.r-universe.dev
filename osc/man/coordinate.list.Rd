\name{coordinate.list}
\alias{coordinate.list}
\title{
List of coordinates for clustering
}
\description{
Extracts coordinates of cells with defined cell class from a raster object.
}
\usage{
coordinate.list(raster, cell.class, compare = "") 
}
\arguments{
  \item{raster}{
    raster with values
}
  \item{cell.class}{
    number or vector of cell-values for clustering
}
  \item{compare}{
    character of type "", "g" or "s". If "g" or "s" all coordinates of cells with value greater "g" resp. smaller "s" than cell.class will be extracted
}
}

\details{
 Works also for very large raster, but can take some time.
}
\value{
 Returns a data frame with lat-, long-coordinates
}

\examples{
data("landcover")

coordinate.list(landcover, 1:10)

}
\keyword{utils}
