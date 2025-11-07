\name{MeuseData}
\alias{MeuseData}
\docType{data}
\title{
Objects Defining Meuse River Mesh
}
\description{
The data are the points, edges and triangles required to mesh a bank of the 
Meuse river in Belgium that is of concern for heavy metal polution. The data
to be analyzed are locations and size of zinc assays.  A covariate is distances
of locations from the river.
}
\usage{
MeuseData
}
\format{A named list.}
\details{
  A named list with four members. The number of points is 155, the number of edges 
  is 52, and the number of triangles is 257.
  \describe{
      \item{covdata:}{A 155 by 2 matrix.  The first column contains the 
      distance from the river of each location, and the second the amount of 
      zinc detected.}  
      \item{pts:}{A 155 by 2 matrix containing the locations of the deposits.}
      \item{edg:}{A 52 by 2 matrix containing the indices of the points that
      are the beginning and end of each of the 52 boundary segments.}
      \item{tri:}{A 257 by 3 matrix containing the indices of the vertices of each
      triangle in counter-clockwise order.}
    }
}
\keyword{datasets}
