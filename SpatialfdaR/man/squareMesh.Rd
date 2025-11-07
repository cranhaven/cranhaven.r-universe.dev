\name{squareMesh}
\alias{squareMesh}
\title{
   Generate a Triangulation of a Square.
}
\description{
   The square is subdivided into \code{m} internal squares.  
   Within each internal square nodes are set at the corners and the middle.  
   Each square is thus subdivided into four triangles.
}
\usage{
  squareMesh(m=1)
}
\arguments{
  \item{m}{The number of squares within the outer square.}
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
\examples{
m <- 3
#  A square of size 3 with 9 internal squares, 
#  25 nodes, 12 edge segments and 36 triangles.
result = squareMesh(m)
pts    = result$pts
edg    = result$edg
tri    = result$tri
#  plot the mesh
plotFEM.mesh(pts,tri)
}
