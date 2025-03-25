##~~ Triakis octahedron ~~##
library(gyro)
library(rgl)

C0 <- 1 + sqrt(2)
vertices <- rbind(
  V0  = c( 0.0,  0.0,   C0),
  V1  = c( 0.0,  0.0,  -C0),
  V2  = c(  C0,  0.0,  0.0),
  V3  = c( -C0,  0.0,  0.0),
  V4  = c( 0.0,   C0,  0.0),
  V5  = c( 0.0,  -C0,  0.0),
  V6  = c( 1.0,  1.0,  1.0),
  V7  = c( 1.0,  1.0, -1.0),
  V8  = c( 1.0, -1.0,  1.0),
  V9  = c( 1.0, -1.0, -1.0),
  V10 = c(-1.0,  1.0,  1.0),
  V11 = c(-1.0,  1.0, -1.0),
  V12 = c(-1.0, -1.0,  1.0),
  V13 = c(-1.0, -1.0, -1.0)
)

faces <- 1 + rbind(
  c(  6,  0,  2 ),
  c(  6,  2,  4 ),
  c(  6,  4,  0 ),
  c(  7,  1,  4 ),
  c(  7,  4,  2 ),
  c(  7,  2,  1 ),
  c(  8,  0,  5 ),
  c(  8,  5,  2 ),
  c(  8,  2,  0 ),
  c(  9,  1,  2 ),
  c(  9,  2,  5 ),
  c(  9,  5,  1 ),
  c( 10,  0,  4 ),
  c( 10,  4,  3 ),
  c( 10,  3,  0 ),
  c( 11,  1,  3 ),
  c( 11,  3,  4 ),
  c( 11,  4,  1 ),
  c( 12,  0,  3 ),
  c( 12,  3,  5 ),
  c( 12,  5,  0 ),
  c( 13,  1,  5 ),
  c( 13,  5,  3 ),
  c( 13,  3,  1 )
)

edges <- do.call(rbind, lapply(1:nrow(faces), function(i){
  f <- sort(faces[i, ])
  rbind(
    c(f[1L], f[2L]),
    c(f[1L], f[3L]),
    c(f[2L], f[3L])
  )
}))
edges <- edges[!duplicated(edges), ]

s <- 0.7

open3d(windowRect = c(50, 50, 562, 562), zoom = 0.65)
bg3d(rgb(54, 57, 64, maxColorValue = 255))
light3d(diffuse = "orangered")
for(i in 1L:nrow(faces)){
  face <- faces[i, ]
  v1 <- vertices[face[1L], ]
  v2 <- vertices[face[2L], ]
  v3 <- vertices[face[3L], ]
  mesh <- gyrotriangle(
    v1, v2, v3, s = s,
    palette = hcl.colors(256L, palette = "Spectral"),
    bias = 1.5, interpolate = "spline", g = function(u) u^2
  )
  shade3d(mesh, specular = "violetred")
}
for(i in 1L:nrow(edges)){
  idx <- edges[i, ]
  A <- vertices[idx[1L], ]
  B <- vertices[idx[2L], ]
  edge <- gyrotube(A, B, s = s, radius = 0.03)
  shade3d(edge, color = "deeppink")
}
spheres3d(vertices, radius = 0.04, color = "deeppink")
