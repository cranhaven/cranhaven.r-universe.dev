library(rgl)
library(cxhull)
library(gyro)

##~~ Barth-like surface ~~##

# icosidodecahedron
phi <- (1+sqrt(5))/2
vs1 <- rbind(
  c(0, 0, 2*phi),
  c(0, 2*phi, 0),
  c(2*phi, 0, 0)
)
vs1 <- rbind(vs1, -vs1)
vs2 <- rbind(
  c(1, phi, phi^2),
  c(1, phi, -phi^2),
  c(1, -phi, phi^2),
  c(-1, phi, phi^2),
  c(1, -phi, -phi^2),
  c(-1, phi, -phi^2),
  c(-1, -phi, phi^2),
  c(-1, -phi, -phi^2)
)
vs2 <- rbind(vs2, vs2[, c(2L, 3L, 1L)], vs2[, c(3L, 1L, 2L)])
vs <- rbind(vs1, vs2)

# triangular faces and their centers
h <- cxhull(vs)
nvertices <- sapply(h[["facets"]], function(f) length(f[["vertices"]]))
indices <- which(nvertices == 3L)
centers <-
  t(vapply(indices, function(i) h[["facets"]][[i]][["center"]], numeric(3L)))

# mesh
nindices <- length(indices)
faces <- matrix(integer(3L*4L*nindices), ncol = 3L, nrow = 4L*nindices)
for(j in 1L:nindices){
  v1 <- vertices[1L, j]
  v2 <- vertices[2L, j]
  v3 <- vertices[3L, j]
  v4 <- j + 30L
  faces[4L*(j-1L)+1L, ] <- c(v1, v2, v3)
  faces[4L*(j-1L)+2L, ] <- c(v1, v2, v4)
  faces[4L*(j-1L)+3L, ] <- c(v1, v3, v4)
  faces[4L*(j-1L)+4L, ] <- c(v2, v3, v4)
}
vertices <- rbind(vs, centers*1.5)
edges <- do.call(rbind, lapply(1L:nrow(faces), function(i){
  f <- faces[i, ]
  rbind(
    c(f[1L], f[2L]),
    c(f[1L], f[3L]),
    c(f[2L], f[3L])
  )
}))
edges <- edges[!duplicated(edges), ]


# plot
s <- 1
open3d(windowRect = c(50, 50, 562, 562))
view3d(0, 0, zoom = 0.65)
for(i in 1L:nrow(faces)){
  idx <- faces[i, ]
  mesh <- gyrotriangle(
    vertices[idx[1L], ], vertices[idx[2L], ], vertices[idx[3L], ], s = s
  )
  shade3d(mesh, color = "darkslateblue")
}
for(i in 1L:nrow(edges)){
  idx <- edges[i, ]
  A <- vertices[idx[1L], ]; B <- vertices[idx[2L], ]
  tube <- gyrotube(A, B, s = s, n = 100, radius = 0.05)
  shade3d(tube, color = "darkgoldenrod")
}
spheres3d(vertices, radius = 0.075, color = "darkgoldenrod")
