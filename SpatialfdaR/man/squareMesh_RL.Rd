\name{squareMesh_RL}
\alias{squareMesh_RL}
\title{
   Generate a Triangulation of a Square.
}
\description{
   The square is subdivided into \code{m} internal squares.  Within each internal square,
   nodes are set at the corners.  Each square is subdivided into two triangles by a 
   line either going from lower left to upper right if \code{orientation="L"}, of by
   a line from lower right to upper left is \code{orientation="R"}.
}
\usage{
  squareMesh_RL(m=1,orientation="L")
}
\arguments{
  \item{m}{The number of squares within the outer square.}
  \item{orientation}{The position of the lower corner of the diagonal line
  subdividing each square.  This must be either \code{"L"} or \code{"R"}.}
}
\details{
  The outer square has sides of length \code{m}.  If another length $x > 0$ of the side
  is required, this can easily be achieved by the command \code{p = p*x/m}.
}
\value{
  A named list containing locations of nodes and which nodes define
  the edge segments and the triangles.  The members are:
  \itemize{
    \item{p:} {A two-column matrix specifying the nodes of the mesh.} 
    \item{e:} {A two-column matrix of integers specifying which nodes define 
    each edge segment.} 
    \item{t:} {A three-column matrix of integers specifying which nodes
    define each triangle in anti-clockwise order.}
  }
}
\references{
  Sangalli, Laura M., Ramsay, James O., Ramsay, Timothy O. (2013),
    Spatial spline regression models,
    Journal of the Royal Statistical Society, Series B, 75, 681-703.
}
\author{
  Jim Ramsay
}
\seealso{
  \code{link{plotFEM.mesh}}
}
