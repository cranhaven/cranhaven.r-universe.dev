makenodes <- function(pts, tri, order=2) {
#MAKENODES produces:
#  a matrix NODES containing coordinates for all of the nodes to be used,
#  a matrix NODEINDEX defining which nodes correspond to each element.
#  If NORDER is 2, the midpoint of each edge is computed and added to
#  POINTS to obtain matrix NODES.
#  The row index of that midpoint is then added to the rows of TRIANGLES
#  containing that edge to define NODEINDEX.
#  If NORDER is 1, nodes corresponds to vertices and NODEINDEX is
#  identical to TRIANGLES.
#
#  nvert:  number of vertices
#  nele:   number of triangles or elements
#
#Input: POINTS is an nvert by 2 matrix containing the x and y
#   coordinates of each of the nvert points in the right-hand rule mesh.
#   POINTS = P' where P is the points matrix for pde
#   The call can use P directly (see below).
#   TRIANGLES is T(1:3,:)' where T is the triangle index matrix from pde.
#   Vertices must be numbered in the counterclockwise direction.
#   NORDER is the order of elements, and may be either 1 or 2 (default)
#
#Output:
#     NODES:  a numnodes*2 matrix whose i'th row contains
#       the coordinates of the i'th nodal variable.
#       Nodes for the second order element consist of vertices and
#       midpoints of edges, that is, 6 per triangle.
#       The first NVER rows of NODES is POINTS, and the remainder are
#       the edge midpoints.
#       Nodes for the first order element consist of only vertices.
#
#     NODEINDEX:  for NORDER == 2, an nele*6 matrix whose i'th row
#       contains the row numbers (in NODES) of the
#       nodal variables defining the i'th finite
#       element.  If the i'th row of FMESH is [V1 V2 V3]
#       then the i'th row of nodeindex is
#       [V1 V(12) V2 V(23) V3 V(31)], where Vi is the
#       row number of the i'th point and V(ij) is the
#       row number of the midpoint of the edge defined
#       by the i'th and j'th points.
#       If NORDER == 1, NODEINDEX is TRIANGLES.
#
#  Last modified 9 July 2019 by Jim Ramsay.

#
#  The first rows of nodes are the vertices

if  (dim(pts)[2] > 2) {
  nodes=tri[pts]
  tri=t(tri[1:3,])
} else {
  nodes <- pts
}

nele <- dim(tri)[1]
nver <- dim(pts)[1]

Jvec   <- matrix(0,nele,1)      #  vector of jacobian values
metric <- array(0,c(nele,2,2))  #  3-d array of metric matrices

if (order == 2) {
  rec  <- matrix (0, nrow=nver, ncol=nver)
  ind  <- rbind( c(1,2), c(2,3), c(3,1) )
  nodeindex <- matrix (0, nrow=nele, ncol=6)
  nodeindex[,c(1,3,5)] <- tri[,c(1,2,3)]
  for (i in 1:nele) {
  	for (j in 1:3) {
  	  if (rec[tri[i,ind[j,1]],tri[i,ind[j,2]]]==0) {
  	    nodes <- rbind(nodes, as.vector(t(c(.5,.5))) %*%
  			        as.matrix(nodes[tri[i,ind[j,]],]))
        rec[tri[i,ind[j,2]],tri[i,ind[j,1]]] <- dim(nodes)[[1]]
        nodeindex[i,2*j] <- dim(nodes)[[1]]
      } else {
        nodeindex[i,2*j] <- rec[tri[i,ind[j,1]],tri[i,ind[j,2]]]
      }
    }
  }

  #  deviations of vertices 2 and 3 from vertex 1

  diff1x <- nodes[nodeindex[i,3],1] - nodes[nodeindex[i,1],1]
  diff1y <- nodes[nodeindex[i,3],2] - nodes[nodeindex[i,1],2]
  diff2x <- nodes[nodeindex[i,5],1] - nodes[nodeindex[i,1],1]
  diff2y <- nodes[nodeindex[i,5],2] - nodes[nodeindex[i,1],2]

  #  Jacobian or area of triangle

  Jvec[i] <- (diff1x*diff2y - diff2x*diff1y)/2

  #  Compute controvariant transformation matrix

  Ael <- matrix(c(diff2y, -diff1y, -diff2x,  diff1x),
               nrow=2,ncol=2,byrow=T)/Jvec[i]

  #  Compute metric matrix

  metric[i,,] <- t(Ael) %*% Ael

} else {
  if (order == 1) {
    nodeindex <- matrix(tri,nele,3)
    for (i in 1:nele) {

  	  #  deviations of vertices 2 and 3 from vertex 1

      diff1x <- nodes[nodeindex[i,2],1] - nodes[nodeindex[i,1],1]
      diff1y <- nodes[nodeindex[i,2],2] - nodes[nodeindex[i,1],2]
      diff2y <- nodes[nodeindex[i,3],2] - nodes[nodeindex[i,1],2]
      diff2x <- nodes[nodeindex[i,3],1] - nodes[nodeindex[i,1],1]

      #  Jacobian or area of triangle

      Jvec[i] <- (diff1x*diff2y - diff2x*diff1y)/2

      #  Compute contravariant transformation matrix

      Ael <- matrix(c(diff2y, -diff1y, -diff2x,  diff1x),
                 nrow=2,ncol=2,byrow=T)/Jvec[i]

      #  Compute metric matrix

      metric[i,,] <- t(Ael) %*% Ael
    }
  } else {
    stop("ORDER not 1 or 2")
  }
}


nodeList <- list(order=order, nodes=nodes, nodeindex=nodeindex,
                  J=Jvec, metric=metric)
return(nodeList)
}
