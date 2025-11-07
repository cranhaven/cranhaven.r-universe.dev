create.FEM.basis <- function(pts, edg=NULL, tri, order=1, nquad=0) {
#
#  CREATE.FEM.BASIS sets up a finite element basis for the analysis
#  of spatial data.  It requires a triangular mesh as input.
#  The triangular mesh is defined by a Delaunay triangulation of the domain
#  of the basis functions, along with, optionally, a decomposition of the
#  domain into subdomains (optional).
#  The finite elements used for functional data analysis are first or second
#  order Lagrangian elements.  These are triangles covering a region,
#  and the basis system is piecewise polinomials (linear or quadratic). There is a basis
#  function associated with each node in the system.
#  When ORDER = 1 the basis system is piecewise linear and the nodes are the vertices of the triangles.
#  When ORDER = 2 the basis system is piecewise quadratic and the nodes are points that are etiher
#  the vertices of the triangles or midpoints of edges of triangles.
#
#  Arguments:
#  PTS  ... The NBASIS by 2 matrix of vertices of triangles containing
#           the X- and Y-coordinates of the vertices.
#           PTS may also be provided as a 2 by NBASIS matrix.
#  EDG  ... The number of edges by 2 matrix defining the segments of the
#           boundary of the region which are also edges of the triangles
#           adjacent to the boundary.
#           The values in matrix EDG are the indices of the vertices in
#           matrix PTS of the starting and ending points of the edges.
#  TRI  ... The no. of triangles by 3matrix specifying triangles and
#           their properties. The indices in PTS of the vertices of each
#           triangle are in counter-clockwise order.
#  ORDER.   Order of elements, which may be either 1 or 2 (2 is default)
#  NQUAD    Number of quadrature points to be used for integrating over triangles.
#
#  Returns:
#  An object of the basis class with parameters stored in member params,
#  which in this case is a list object with members p, e and t.

#  Last modified 20 November 2021 by Jim Ramsay.

#  check dimensions of PTS, EDG and TRI and transpose if necessary

ndim <- dim(tri)
ntri <- ndim[1]
ncol <- ndim[2]

if  (is.null(edg)) {
  if (dim(pts)[[2]] != 2 || dim(tri)[[2]] != 3) {
      stop('Dimensions of at least one of pts and tri are not correct.')
  }
} else {
  if (dim(pts)[[2]] != 2 || dim(edg)[[2]] != 2 || dim(tri)[[2]] != 3) {
    stop('Dimensions of at least one of pts, edg and tri are not correct.')
  }
}

type <- "FEM"

#  Argument rangeval is not needed for an FEM basis since domain
#  boundary is defined by the triangular mesh.

rangeval <- c(0,1)

#  The number of basis functions corresponds to the number of vertices
#  for order = 1, and to vertices plus edge midpoints for order = 2

#  set up the nodal points and corresponding mesh:
#    this is p' and t(1:3,:)' in the order 1 case, but
#    includes mid-points in the order 2 case

nodeList <- makenodes(pts,tri,order)

#  set up quadrature points and weights for integration over triangles
#  if nquad is provided, else default to NULL

if (!is.null(nquad) && nquad > 0) {
  quadList <-  vector("list",ntri)
  for (itri in 1:ntri) {
    quadList[[itri]] <-  triquad(nquad,tri[itri,1:3])
  }
} else {
	quadList <-NULL
}

#  The params argument is a list object

petList=NULL
petList$p         <-  pts
petList$e         <-  edg
petList$t         <-  tri
petList$order     <-  order
petList$nodes     <-  nodeList$nodes
petList$nodeindex <-  nodeList$nodeindex
petList$J         <-  nodeList$J
petList$metric    <-  nodeList$metric
petList$quadmat   <-  quadList

params = petList

nbasis = dim(nodeList$nodes)[1]

basisobj = basisfd(type, rangeval, nbasis, params)

return(basisobj)

}

