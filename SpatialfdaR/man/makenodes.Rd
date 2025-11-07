\name{makenodes}
\alias{makenodes}
\title{
  Compute the matrix \code{nodes} containing all the nodes in the mesh.
}
\description{
  Each basis function is associated with a node.  If the order of the finite
  elements is 1, the nodes are the vertices of the triangles.  If the order
  is 2, the nodes are also at the midpoints of the edges.
}
\usage{
  makenodes(pts, tri, order=2)
}
\arguments{
  \item{pts}{
    A two-column matrix of the locations of the vertices of the mesh.
  }
  \item{tri}{
    A three-column matrix of the indices in \code{pts} of the triangle vertices.
  }
  \item{order}{
    The order of the finite element, which is either 1 or 2.
  }
}
\details{
  Computes a matrix nodes containing coordinates for all of the nodes to be used,
  a matrix nodeindex defining which nodes correspond to each element.
  If norder is 1, nodes corresponds to vertices and nodeindex is
  identical to triangles.
  If norder is 2, the midpoint of each edge is computed and added to
  points to obtain matrix nodes.
  The row index of that midpoint is then added to the rows of triangles
  containing that edge to define nodeindex.
}
\value{
  A list object containing these members:
  \itemize{
  	\item{order}{
  	  The order of the finite elements, which is either 1 or 2.
  	}
  	\item{nodes}{
  	   A numnodes*2 matrix whose i'th row contains
       the coordinates of the i'th nodal variable.
       Nodes for the second order element consist of vertices and
       midpoints of edges, that is, 6 per triangle.
       Points are listed first followed by midpoints.
       Nodes for the first order element consist of only vertices.
     }
     \item{nodeindex}{
       If norder == 1, nodeindex is triangles.
       If norder == 2, an nele*6 matrix whose i'th row
       contains the row numbers (in nodes) of the
       nodal variables defining the i'th finite
       element.  If the i'th row of FMESH is [V1 V2 V3]
       then the i'th row of nodeindex is
       [V1 V(12) V2 V(23) V3 V(31)], where Vi is the
       row number of the i'th point and V(ij) is the
       row number of the midpoint of the edge defined
       by the i'th and j'th points.
       If norder == 1, nodeindex is triangles
     }
     \item{jvec}{
       Jacobian, which is the area of triangle.
     }
     \item{metric}{
       Metric matrix.
     }
  }
}
\seealso{
  \code{\link{mass.FEM}},
  \code{\link{stiff.FEM}}
}
\keyword{makenodes}
