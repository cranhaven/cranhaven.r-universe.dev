library(gyro)
library(rgl)
library(Rvcg)
library(Morpho)

##~~ Great deltoidal icositetrahedron ~~##

C0 <- (4 - sqrt(2)) / 7
C1 <- sqrt(2)
vertices <- rbind(
  c( 0.0,  0.0,   C1),
  c( 0.0,  0.0,  -C1),
  c(  C1,  0.0,  0.0),
  c( -C1,  0.0,  0.0),
  c( 0.0,   C1,  0.0),
  c( 0.0,  -C1,  0.0),
  c(-1.0,  0.0, -1.0),
  c(-1.0,  0.0,  1.0),
  c( 1.0,  0.0, -1.0),
  c( 1.0,  0.0,  1.0),
  c(-1.0, -1.0,  0.0),
  c(-1.0,  1.0,  0.0),
  c( 1.0, -1.0,  0.0),
  c( 1.0,  1.0,  0.0),
  c( 0.0, -1.0, -1.0),
  c( 0.0, -1.0,  1.0),
  c( 0.0,  1.0, -1.0),
  c( 0.0,  1.0,  1.0),
  c( -C0,  -C0,  -C0),
  c( -C0,  -C0,   C0),
  c( -C0,   C0,  -C0),
  c( -C0,   C0,   C0),
  c(  C0,  -C0,  -C0),
  c(  C0,  -C0,   C0),
  c(  C0,   C0,  -C0),
  c(  C0,   C0,   C0)
)
faces <- 1 + rbind(
  c(  0,  6, 18, 14 ),
  c(  0, 14, 22,  8 ),
  c(  0,  8, 24, 16 ),
  c(  0, 16, 20,  6 ),
  c(  1,  9, 23, 15 ),
  c(  1, 15, 19,  7 ),
  c(  1,  7, 21, 17 ),
  c(  1, 17, 25,  9 ),
  c(  2,  7, 19, 10 ),
  c(  2, 10, 18,  6 ),
  c(  2,  6, 20, 11 ),
  c(  2, 11, 21,  7 ),
  c(  3,  8, 22, 12 ),
  c(  3, 12, 23,  9 ),
  c(  3,  9, 25, 13 ),
  c(  3, 13, 24,  8 ),
  c(  4, 10, 19, 15 ),
  c(  4, 15, 23, 12 ),
  c(  4, 12, 22, 14 ),
  c(  4, 14, 18, 10 ),
  c(  5, 11, 20, 16 ),
  c(  5, 16, 24, 13 ),
  c(  5, 13, 25, 17 ),
  c(  5, 17, 21, 11 )
)

# edges
edges <- do.call(rbind, lapply(split(faces, 1L:24L), function(p){
  rbind(
    c(p[1L], p[2L]),
    c(p[2L], p[3L]),
    c(p[3L], p[4L]),
    c(p[4L], p[1L])
  )
}))
edges <- edges[!duplicated(t(apply(edges, 1L, sort))), ]


# draw
s <- 0.6
open3d(windowRect = c(50, 50, 562, 562))
bg3d(color = "#363940")
view3d(zoom = 0.6)
for(i in 1L:nrow(faces)){
  pts <- vertices[faces[i, ], ]
  m1 <- gyrotriangle(pts[1L, ], pts[2L, ], pts[3L, ], s)
  m2 <- gyrotriangle(pts[1L, ], pts[3L, ], pts[4L, ], s)
  mesh <- vcgClean(mergeMeshes(m1, m2), sel = c(0, 7), silent = TRUE)
  shade3d(mesh, color = "violetred")
}
for(i in 1L:nrow(edges)){
  idx <- edges[i, ]
  A <- vertices[idx[1L], ]; B <- vertices[idx[2L], ]
  edge <- gyrotube(A, B, s = s, radius = 0.01)
  shade3d(edge, color = "whitesmoke")
}
spheres3d(vertices, radius = 0.02, color = "whitesmoke")
