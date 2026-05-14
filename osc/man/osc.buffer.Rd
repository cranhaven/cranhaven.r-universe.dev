\name{osc.buffer}
\alias{osc.buffer}
\title{
Simple Buffer algorithm
}
\description{
Simple buffer based on euclidean distance are created around all cells equal to one.
}
\usage{
osc.buffer(input, width=max(dim(input)), return.width=F, complete=F) 
}
\arguments{
  \item{input}{
    Matrix or Raster containing 1 indicating a cluster, no NA values are allowed
}
  \item{width}{
    Width of the buffer in cells
}
  \item{return.width}{
    Logical value, if TRUE then the distance to the nearest cell with value 1 will be returned for all cells within the buffer.
}
  \item{complete}{
    Logical value, if TRUE the buffer will be increased until the whole raster is covered. Can only be used in combination with return.width=TRUE.
}
}

\value{
 Returns matrix or raster, depending on input, with 1 for cluster and -1 for surrounding buffer. If return.width=TRUE negative numbers indicating the distance (width) to the nearest cell with value 1.
}

\examples{
data(landcover)
landcover[landcover[]>1] <- 0

plot(osc.buffer(landcover, 6))

plot(osc.buffer(landcover, 6, return.width = TRUE))

plot(osc.buffer(landcover, return.width = TRUE, complete = TRUE))

}
\keyword{utils}
