####~~ translated icosahedra ~~####
library(gyro)
library(rgl)
library(Morpho)
library(Rvcg)

# draw hyperbolic polyhedron ####
hpolyhedron <- function(vertices, faces, edges, s){
  Gtriangles <- vector("list", nrow(faces))
  for(i in 1:nrow(faces)){
    idx <- faces[i, ]
    Gtriangles[[i]] <- gyrotriangle(
      vertices[idx[1], ], vertices[idx[2], ], vertices[idx[3], ], s = s
    )
  }
  mesh <- vcgClean(mergeMeshes(Gtriangles), sel = 0)
  shade3d(mesh, color = "royalblue")
  for(i in 1:nrow(edges)){
    idx <- edges[i, ]
    A <- vertices[idx[1], ]; B <- vertices[idx[2], ]
    tube <- gyrotube(A, B, s = s, n = 100, radius = 0.03)
    shade3d(tube, color = "powderblue")
  }
  spheres3d(vertices, radius = 0.05, color = "powderblue")
}

# vertices ####
icosahedron <- icosahedron3d()
vertices <- t(icosahedron$vb[-4, ])

# faces ####
faces <- t(icosahedron$it)

# edges ####
edges <- as.matrix(vcgGetEdge(icosahedron)[, c("vert1", "vert2")])

# plot ####
open3d(windowRect = c(50, 50, 562, 562))
view3d(0, 0, zoom = 0.9)
s <- 0.5
h <- 1.5
hpolyhedron(
  sweep(vertices, 2, c(h, 0, 0), "+"), faces, edges, s = s
)
hpolyhedron(
  sweep(vertices, 2, c(h*cos(2*pi/3), h*sin(2*pi/3), 0), "+"),
  faces, edges, s = s
)
hpolyhedron(
  sweep(vertices, 2, c(h*cos(4*pi/3), h*sin(4*pi/3), 0), "+"),
  faces, edges, s = s
)
