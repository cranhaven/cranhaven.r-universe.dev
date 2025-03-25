library(gyro)
library(rgl)
library(Rvcg)
library(Morpho)
library(cxhull)

##~~ rhombic triacontahedron ~~##

# The rhombic triacontahedron can be obtained as the convex hull
#   of a 3D projection of the 6-cube.

# vertices of the 6-cube
SixCubeVertices <- as.matrix(expand.grid(rep(list(c(-1, 1)), 6L)))

# the three 3D vectors spanning the space of the projection
phi <- (1 + sqrt(5)) / 2
u1 <- c(  1, phi,   0,  -1, phi,   0)
u2 <- c(phi,   0,   1, phi,   0,  -1)
u3 <- c(  0,   1, phi,   0,  -1, phi)
# let's normalize them
u1 <- u1 / sqrt(c(crossprod(u1)))
u2 <- u2 / sqrt(c(crossprod(u2)))
u3 <- u3 / sqrt(c(crossprod(u3)))

# the projection
proj <- function(v){
  c(c(crossprod(v, u1)), c(crossprod(v, u2)), c(crossprod(v, u3)))
}
vertices <- t(apply(SixCubeVertices, 1L, proj))

# now we compute the convex hull
hull <- cxhull(vertices, triangulate = FALSE)
edges <- hull[["edges"]]

# we find 32 vertices and 30 faces:
str(hull, max = 1L)
# List of 5
#  $ vertices:List of 32
#  $ edges   : int [1:60, 1:2] 1 1 1 9 9 9 9 10 10 13 ...
#  $ ridges  :List of 60
#  $ facets  :List of 30
#  $ volume  : num 34.8
hullVertices <- t(vapply(hull[["vertices"]], function(vertex){
  vertex[["point"]]
}, numeric(3L)))

# all the faces have four vertices (each face is a rhombus):
nvertices_per_face <- vapply(hull[["facets"]], function(face){
  length(face[["vertices"]])
}, integer(1L))
all(nvertices_per_face == 4L)

# to split them into two triangles, we need to order their vertices
polygonize <- function(edges){ # function which orders the vertices
  nedges <- nrow(edges)
  es <- edges[1L, ]
  i <- es[2L]
  edges <- edges[-1L, ]
  for(. in 1L:(nedges-2L)){
    j <- which(apply(edges, 1L, function(e) i %in% e))
    i <- edges[j, ][which(edges[j, ] != i)]
    es <- c(es, i)
    edges <- edges[-j, ]
  }
  es
}

rhombuses <- t(vapply(
  hull[["facets"]],
  function(face) polygonize(face[["edges"]]),
  integer(4L)
))

# observe the distances between all pairs of vertices of a rhombus:
rhombusVertices <- vertices[rhombuses[1L, ], ]
dist(rhombusVertices)
#>          1        2        3
#> 2 1.414214
#> 3 2.406004 1.414214
#> 4 1.414214 1.486992 1.414214
# the edges have the same length
#   (a rhombus is also called "equilateral quadrilateral")
#   but there's a long diagonal and a short diagonal
# we will split the rhombuses along the long diagonal,
#   for a better rendering of the hyperbolic polyhedron


# draw ####
s <- 0.8
open3d(windowRect = c(50, 50, 562, 562), zoom = 0.7)
bg3d(rgb(54, 57, 64, maxColorValue = 255))
for(i in 1L:nrow(rhombuses)){
  rhombus <- rhombuses[i, ]
  rhombusVertices <- vertices[rhombus, ]
  # here is how we choose to split along the long diagonal:
  d <- c(round(dist(rhombusVertices)))
  if(d[5L] == 2){
    rhombusVertices <- vertices[rev(rhombus), ]
  }
  mesh1 <- gyrotriangle(
    rhombusVertices[1L, ], rhombusVertices[2L, ], rhombusVertices[3L, ], s = s
  )
  mesh2 <- gyrotriangle(
    rhombusVertices[1L, ], rhombusVertices[3L, ], rhombusVertices[4L, ], s = s
  )
  mesh <- vcgClean(mergeMeshes(mesh1, mesh2), sel = c(0, 7), silent = TRUE)
  shade3d(mesh, color = "darkmagenta")
}
for(i in 1L:nrow(edges)){
  ids <- edges[i, ]
  A <- vertices[ids[1L], ]; B <- vertices[ids[2L], ]
  tube <- gyrotube(A, B, s = s, radius = 0.025)
  shade3d(tube, color = "whitesmoke")
}
spheres3d(hullVertices, radius = 0.03, color = "whitesmoke")
