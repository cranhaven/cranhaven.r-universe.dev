\name{plotFEM.mesh}
\alias{plotFEM.mesh}
\title{
  Plot a finite element mesh.
}
\description{
  A finite element mesh is a set of triangles defining a two-dimensional region.
}
\usage{
  plotFEM.mesh(pts, tri, xlabel="x", ylabel="y", xlim=plim1, ylim=plim2, 
               shift=0.05, nonum=TRUE)
}
\arguments{
  \item{pts}{A two-column matrix of the locations of the vertices of the mesh.}
  \item{tri}{
    A three-column matrix of the indices in \code{pts} of the triangle vertices.
  }
  \item{xlabel}{A character string for the label of the abscissa or horizontal axis.}
  \item{ylabel}{A character string for the label of the ordinate or vertical axis.}
  \item{xlim}{A vector of length 2 containing the left and right plotting limits, 
  respectively.}
  \item{ylim}{A vector of length 2 containing the bottom and plot plotting limits, 
  respectively.}
  \item{shift}{A lateral character shift for text plotting.}
  \item{nonum}{A logical constant.  If FALSE, numbers for nodes and triangles will
  not be displayed.}
}
\value{
  The nodes and mesh lines are plotted.  The number of each node and triangle 
  is plotted, with node numbers in black and triangle number in blue.
}
\seealso{
  \code{\link{plotFEM.fd}}
}
\examples{
#  Set up and plot a square with side length one with 32 interior triangles,
#  and then create an order one functional basis object of type FEM.
m <- 4
petList <- squareMesh(m)
pts <- petList$p
edg <- petList$e
tri <- petList$t
pts <- pts/m
plotFEM.mesh(pts, tri)
}
