library(gyro)
library(rgl)
library(Rvcg)
library(Morpho)

##~~ Truncated great dodecahedron ~~##

C0 <- (sqrt(5) - 1) / 4
C1 <- (1 + sqrt(5)) / 4
C2 <- (3 + sqrt(5)) / 4
C3 <- (1 + sqrt(5)) / 2
C4 <- (5 + sqrt(5)) / 4
vertices <- rbind(
  c( 0.5,  0.0,   C4),
  c( 0.5,  0.0,  -C4),
  c(-0.5,  0.0,   C4),
  c(-0.5,  0.0,  -C4),
  c(  C4,  0.5,  0.0),
  c(  C4, -0.5,  0.0),
  c( -C4,  0.5,  0.0),
  c( -C4, -0.5,  0.0),
  c( 0.0,   C4,  0.5),
  c( 0.0,   C4, -0.5),
  c( 0.0,  -C4,  0.5),
  c( 0.0,  -C4, -0.5),
  c(  C1,  0.5,   C3),
  c(  C1,  0.5,  -C3),
  c(  C1, -0.5,   C3),
  c(  C1, -0.5,  -C3),
  c( -C1,  0.5,   C3),
  c( -C1,  0.5,  -C3),
  c( -C1, -0.5,   C3),
  c( -C1, -0.5,  -C3),
  c(  C3,   C1,  0.5),
  c(  C3,   C1, -0.5),
  c(  C3,  -C1,  0.5),
  c(  C3,  -C1, -0.5),
  c( -C3,   C1,  0.5),
  c( -C3,   C1, -0.5),
  c( -C3,  -C1,  0.5),
  c( -C3,  -C1, -0.5),
  c( 0.5,   C3,   C1),
  c( 0.5,   C3,  -C1),
  c( 0.5,  -C3,   C1),
  c( 0.5,  -C3,  -C1),
  c(-0.5,   C3,   C1),
  c(-0.5,   C3,  -C1),
  c(-0.5,  -C3,   C1),
  c(-0.5,  -C3,  -C1),
  c(  C2,   C0,   C2),
  c(  C2,   C0,  -C2),
  c(  C2,  -C0,   C2),
  c(  C2,  -C0,  -C2),
  c( -C2,   C0,   C2),
  c( -C2,   C0,  -C2),
  c( -C2,  -C0,   C2),
  c( -C2,  -C0,  -C2),
  c(  C2,   C2,   C0),
  c(  C2,   C2,  -C0),
  c(  C2,  -C2,   C0),
  c(  C2,  -C2,  -C0),
  c( -C2,   C2,   C0),
  c( -C2,   C2,  -C0),
  c( -C2,  -C2,   C0),
  c( -C2,  -C2,  -C0),
  c(  C0,   C2,   C2),
  c(  C0,   C2,  -C2),
  c(  C0,  -C2,   C2),
  c(  C0,  -C2,  -C2),
  c( -C0,   C2,   C2),
  c( -C0,   C2,  -C2),
  c( -C0,  -C2,   C2),
  c( -C0,  -C2,  -C2)
)

decagons <- 1 + rbind(
  c(  0,  2, 42, 26, 51, 35, 31, 47, 22, 38 ),
  c(  1,  3, 41, 25, 48, 32, 28, 44, 21, 37 ),
  c(  2,  0, 36, 20, 45, 29, 33, 49, 24, 40 ),
  c(  3,  1, 39, 23, 46, 30, 34, 50, 27, 43 ),
  c(  4,  5, 47, 31, 59, 19, 17, 57, 29, 45 ),
  c(  5,  4, 44, 28, 56, 16, 18, 58, 30, 46 ),
  c(  6,  7, 50, 34, 54, 14, 12, 52, 32, 48 ),
  c(  7,  6, 49, 33, 53, 13, 15, 55, 35, 51 ),
  c(  8,  9, 57, 17, 43, 27, 26, 42, 16, 56 ),
  c(  9,  8, 52, 12, 38, 22, 23, 39, 13, 53 ),
  c( 10, 11, 55, 15, 37, 21, 20, 36, 14, 54 ),
  c( 11, 10, 58, 18, 40, 24, 25, 41, 19, 59 )
)

pentagramms <- 1 + rbind(
  c(  0, 38, 12, 14, 36 ),
  c(  1, 37, 15, 13, 39 ),
  c(  2, 40, 18, 16, 42 ),
  c(  3, 43, 17, 19, 41 ),
  c(  4, 45, 20, 21, 44 ),
  c(  5, 46, 23, 22, 47 ),
  c(  6, 48, 25, 24, 49 ),
  c(  7, 51, 26, 27, 50 ),
  c(  8, 56, 28, 32, 52 ),
  c(  9, 53, 33, 29, 57 ),
  c( 10, 54, 34, 30, 58 ),
  c( 11, 59, 31, 35, 55 )
)

# edges
edges1 <- do.call(rbind, lapply(split(pentagramms, 1L:12L), function(p){
  rbind(
    c(p[1L], p[2L]),
    c(p[2L], p[3L]),
    c(p[3L], p[4L]),
    c(p[4L], p[5L]),
    c(p[5L], p[1L])
  )
}))
edges2 <- do.call(rbind, lapply(split(decagons, 1L:12L), function(p){
  cbind(p, c(p[-1L], p[1L]))
}))
edges <- rbind(edges1, edges2)
edges <- edges[!duplicated(t(apply(edges, 1L, sort))), ]

# intersections of edges
tintersect <- function(s){
  pts <- vertices[pentagramms[1L, ], ]
  A <- pts[1L, ]; B <- pts[2L, ]; C <- pts[4L, ]; D <- pts[3L, ]
  f <- function(t){
    c(crossprod(gyroABt(A, B, t, s) - gyroABt(C, D, t, s)))
  }
  optim(0.25, f, lower = 0, upper = 0.5, method = "L-BFGS-B")$par
}


# draw ####
s <- 1
open3d(windowRect = c(50, 50, 562, 562), zoom = 0.6)
bg3d(rgb(54, 57, 64, maxColorValue = 255))
for(i in 1L:12L){
  pts <- vertices[decagons[i, ], ]
  meshes <- lapply(2L:9L, function(j){
    gyrotriangle(pts[1L, ], pts[j, ], pts[j+1L, ], s)
  })
  mesh <- vcgClean(mergeMeshes(meshes), sel = c(0, 7), silent = TRUE)
  shade3d(mesh, color = "hotpink")
}
t <- tintersect(s)
for(i in 1L:12L){
  pts <- vertices[pentagramms[i, ], ]
  P1 <- gyroABt(pts[1L, ], pts[2L, ], t, s)
  P2 <- gyroABt(pts[2L, ], pts[3L, ], t, s)
  P3 <- gyroABt(pts[3L, ], pts[4L, ], t, s)
  P4 <- gyroABt(pts[4L, ], pts[5L, ], t, s)
  P5 <- gyroABt(pts[5L, ], pts[1L, ], t, s)
  m1 <- gyrotriangle(pts[1L, ], P1, P3, s)
  m2 <- gyrotriangle(pts[2L, ], P2, P4, s)
  m3 <- gyrotriangle(pts[3L, ], P3, P5, s)
  m4 <- gyrotriangle(pts[4L, ], P4, P1, s)
  m5 <- gyrotriangle(pts[5L, ], P5, P2, s)
  m6 <- gyrotriangle(P1, P3, P5, s)
  m7 <- gyrotriangle(P1, P5, P2, s)
  m8 <- gyrotriangle(P1, P2, P4, s)
  mesh <- vcgClean(
    mergeMeshes(m1, m2, m3, m4, m5, m6, m7, m8),
    sel = c(0, 7), silent = TRUE
  )
  shade3d(mesh, color = "darkmagenta")
}
for(i in 1L:nrow(edges)){
  idx <- edges[i, ]
  A <- vertices[idx[1L], ]; B <- vertices[idx[2L], ]
  tube <- gyrotube(A, B, s = s, radius = 0.025)
  shade3d(tube, color = "orangered")
}
spheres3d(vertices, radius = 0.035, color = "orangered")
