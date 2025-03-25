library(gyro)
library(rgl)
library(Rvcg)
library(Morpho)

##~~ pentagrammic prism ~~##

vs1 <- t(sapply(c(0, 2, 4, 1, 3), function(i){
  c(cos(2*i*pi/5), sin(2*i*pi/5), 0.3)
}))
vs2 <- t(sapply(c(0, 2, 4, 1, 3), function(i){
  c(cos(2*i*pi/5), sin(2*i*pi/5), -0.3)
}))
vertices <- rbind(vs1, vs2)

pentagramms <- rbind(1:5, 6:10)

rectangles <- rbind(
  c(1, 2, 7, 6),
  c(2, 3, 8, 7),
  c(3, 4, 9, 8),
  c(4, 5, 10, 9),
  c(5, 1, 6, 10)
)

edges <- rbind(
  c(1, 2), c(2, 3), c(3, 4), c(4, 5), c(5, 1),
  c(6, 7), c(7, 8), c(8, 9), c(9, 10), c(10, 6),
  c(2, 7), c(3, 8), c(4, 9), c(5, 10), c(1, 6)
)

# intersections of edges
tintersect <- function(s){
  pts <- vertices[pentagramms[1, ], ]
  A <- pts[1, ]; B <- pts[2, ]; C <- pts[4, ]; D <- pts[3, ]
  f <- function(t){
    c(crossprod(gyroABt(A, B, t, s) - gyroABt(C, D, t, s)))
  }
  optim(0.25, f, lower = 0, upper = 0.5, method = "L-BFGS-B")$par
}

s <- 0.5
open3d(windowRect = c(50, 50, 562, 562))
view3d(30, -30, zoom = 0.8)
for(i in 1:nrow(rectangles)){
  pts <- vertices[rectangles[i, ], ]
  m1 <- gyrotriangle(pts[1, ], pts[2, ], pts[3, ], s)
  m2 <- gyrotriangle(pts[1, ], pts[3, ], pts[4, ], s)
  mesh <- vcgClean(mergeMeshes(m1, m2), sel = c(0, 7), silent = TRUE)
  shade3d(mesh, color = "olivedrab3", specular = "springgreen2")
}
t <- tintersect(s)
for(i in 1:2){
  pts <- vertices[pentagramms[i, ], ]
  P1 <- gyroABt(pts[1, ], pts[2, ], t, s)
  P2 <- gyroABt(pts[2, ], pts[3, ], t, s)
  P3 <- gyroABt(pts[3, ], pts[4, ], t, s)
  P4 <- gyroABt(pts[4, ], pts[5, ], t, s)
  P5 <- gyroABt(pts[5, ], pts[1, ], t, s)
  m1 <- gyrotriangle(pts[1, ], P1, P3, s)
  m2 <- gyrotriangle(pts[2, ], P2, P4, s)
  m3 <- gyrotriangle(pts[3, ], P3, P5, s)
  m4 <- gyrotriangle(pts[4, ], P4, P1, s)
  m5 <- gyrotriangle(pts[5, ], P5, P2, s)
  mm1 <- gyrotriangle(P1, P3, P5, s)
  mm2 <- gyrotriangle(P1, P5, P2, s)
  mm3 <- gyrotriangle(P1, P2, P4, s)
  mesh <- vcgClean(
    mergeMeshes(m1, m2, m3, m4, m5, mm1, mm2, mm3), sel = c(0, 7), silent = TRUE
  )
  shade3d(mesh, color = "maroon", specular = "violetred")
}
for(i in 1:nrow(edges)){
  idx <- edges[i, ]
  A <- vertices[idx[1], ]; B <- vertices[idx[2], ]
  edge <- gyrotube(A, B, s, radius = 0.025)
  shade3d(edge, color = "purple4")
}
spheres3d(vertices, radius = 0.035, color = "purple4")
