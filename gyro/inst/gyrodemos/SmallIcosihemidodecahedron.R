library(gyro)
library(rgl)
library(Rvcg)
library(Morpho)

##~~ Small Icosihemidodecahedron ~~##

C0 <- (1 + sqrt(5)) / 4
C1 <- (3 + sqrt(5)) / 4
C2 <- (1 + sqrt(5)) / 2

vertices <- rbind(
  V0  = c( 0.0,  0.0,   C2),
  V1  = c( 0.0,  0.0,  -C2),
  V2  = c(  C2,  0.0,  0.0),
  V3  = c( -C2,  0.0,  0.0),
  V4  = c( 0.0,   C2,  0.0),
  V5  = c( 0.0,  -C2,  0.0),
  V6  = c( 0.5,   C0,   C1),
  V7  = c( 0.5,   C0,  -C1),
  V8  = c( 0.5,  -C0,   C1),
  V9  = c( 0.5,  -C0,  -C1),
  V10 = c(-0.5,   C0,   C1),
  V11 = c(-0.5,   C0,  -C1),
  V12 = c(-0.5,  -C0,   C1),
  V13 = c(-0.5,  -C0,  -C1),
  V14 = c(  C1,  0.5,   C0),
  V15 = c(  C1,  0.5,  -C0),
  V16 = c(  C1, -0.5,   C0),
  V17 = c(  C1, -0.5,  -C0),
  V18 = c( -C1,  0.5,   C0),
  V19 = c( -C1,  0.5,  -C0),
  V20 = c( -C1, -0.5,   C0),
  V21 = c( -C1, -0.5,  -C0),
  V22 = c(  C0,   C1,  0.5),
  V23 = c(  C0,   C1, -0.5),
  V24 = c(  C0,  -C1,  0.5),
  V25 = c(  C0,  -C1, -0.5),
  V26 = c( -C0,   C1,  0.5),
  V27 = c( -C0,   C1, -0.5),
  V28 = c( -C0,  -C1,  0.5),
  V29 = c( -C0,  -C1, -0.5)
)

## faces
dodecagons <- 1 + rbind(
  c(  0,  6, 22, 23,  7,  1, 13, 29, 28, 12 ),
  c(  0,  8, 24, 25,  9,  1, 11, 27, 26, 10 ),
  c(  2, 14,  6, 10, 18,  3, 21, 13,  9, 17 ),
  c(  2, 15,  7, 11, 19,  3, 20, 12,  8, 16 ),
  c(  4, 22, 14, 16, 24,  5, 29, 21, 19, 27 ),
  c(  4, 23, 15, 17, 25,  5, 28, 20, 18, 26 )
)
triangles <- 1 + rbind(
  c(  0,  6, 10 ),
  c(  0, 12,  8 ),
  c(  1,  9, 13 ),
  c(  1, 11,  7 ),
  c( 14, 16,  2 ),
  c( 14, 22,  6 ),
  c( 17, 15,  2 ),
  c( 17, 25,  9 ),
  c( 19, 21,  3 ),
  c( 19, 27, 11 ),
  c( 20, 18,  3 ),
  c( 20, 28, 12 ),
  c( 23,  4, 22 ),
  c( 23, 15,  7 ),
  c( 24,  5, 25 ),
  c( 24, 16,  8 ),
  c( 26,  4, 27 ),
  c( 26, 18, 10 ),
  c( 29,  5, 28 ),
  c( 29, 21, 13 )
)

# edges
edges1 <-
  do.call(rbind, lapply(split(triangles, 1L:nrow(triangles)), function(p){
    rbind(
      c(p[1L], p[2L]),
      c(p[2L], p[3L]),
      c(p[3L], p[1L])
    )
  }))
edges1 <- edges1[!duplicated(t(apply(edges1, 1L, sort))), ]
edges2 <-
  do.call(rbind, lapply(split(dodecagons, 1L:nrow(dodecagons)), function(p){
    cbind(p, c(p[2L:10L], p[1L]))
  }))
edges2 <- edges2[!duplicated(t(apply(edges2, 1L, sort))), ]
edges <- rbind(edges1, edges2)
edges <- edges[!duplicated(t(apply(edges, 1L, sort))), ]

# draw
s <- 0.8
open3d(windowRect = c(50, 50, 562, 562))
bg3d(color = "#363940")
view3d(zoom = 0.6)
for(i in 1L:nrow(triangles)){
  pts <- vertices[triangles[i, ], ]
  mesh <- gyrotriangle(pts[1L, ], pts[2L, ], pts[3L, ], s)
  shade3d(mesh, color = "magenta")
}
for(i in 1L:nrow(dodecagons)){
  pts <- vertices[dodecagons[i, ], ]
  m1 <- gyrotriangle(pts[1L, ], pts[2L, ], pts[3L, ], s)
  m2 <- gyrotriangle(pts[1L, ], pts[3L, ], pts[4L, ], s)
  m3 <- gyrotriangle(pts[1L, ], pts[4L, ], pts[5L, ], s)
  m4 <- gyrotriangle(pts[1L, ], pts[5L, ], pts[6L, ], s)
  m5 <- gyrotriangle(pts[1L, ], pts[6L, ], pts[7L, ], s)
  m6 <- gyrotriangle(pts[1L, ], pts[7L, ], pts[8L, ], s)
  m7 <- gyrotriangle(pts[1L, ], pts[8L, ], pts[9L, ], s)
  m8 <- gyrotriangle(pts[1L, ], pts[9L, ], pts[10L, ], s)
  mesh <- vcgClean(
    mergeMeshes(m1, m2, m3, m4, m5, m6, m7, m8),
    sel = c(0, 7), silent = TRUE
  )
  shade3d(mesh, color = "darkmagenta")
}
for(i in 1L:nrow(edges)){
  idx <- edges[i, ]
  A <- vertices[idx[1L], ]; B <- vertices[idx[2L], ]
  edge <- gyrotube(A, B, s = s, radius = 0.02)
  shade3d(edge, color = "whitesmoke")
}
spheres3d(vertices, radius = 0.03, color = "whitesmoke")
