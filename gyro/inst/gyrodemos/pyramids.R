library(gyro)
library(rgl)
library(cxhull)

## ~~ based on the rhombicosidodecahedron ~~##
phi <- (1 + sqrt(5)) / 2
vs1 <- rbind(
  c(1, 1, phi^3),
  c(phi^2, phi, 2 * phi),
  c(2 + phi, 0, phi^2)
)
vs2 <- rbind(vs1, vs1[, c(2, 3, 1)], vs1[, c(3, 1, 2)]) # even permutations
vertices <- changesOfSign(vs2)

# square faces and their centers ####
h <- cxhull(vertices)
nvertices <- sapply(h[["facets"]], function(f) length(f[["vertices"]]))
indices <- which(nvertices == 4L)
centers <- t(vapply(indices, function(i){
  h[["facets"]][[i]][["center"]]
}, numeric(3L)))

# function to order the indices of the edges ####
polygonize <- function(edges){
  nedges <- nrow(edges)
  es <- edges[1L, ]
  i <- es[2L]
  edges <- edges[-1L, ]
  for(. in 1L:(nedges - 2L)){
    j <- which(apply(edges, 1L, function(e) i %in% e))
    i <- edges[j, ][which(edges[j, ] != i)]
    es <- c(es, i)
    edges <- edges[-j, ]
  }
  es
}

squares <- vapply(indices, function(r){
  polygonize(h[["facets"]][[r]][["edges"]])
}, integer(4L))

# triangular faces ####
faces <- matrix(NA_integer_, ncol = 3L, nrow = 6L*length(indices))
for(j in 1L:length(indices)){
  v1 <- squares[1L, j]
  v2 <- squares[2L, j]
  v3 <- squares[3L, j]
  v4 <- squares[4L, j]
  v5 <- j + nrow(vertices)
  faces[6L*(j - 1L) + 1L, ] <- c(v1, v2, v3)
  faces[6L*(j - 1L) + 2L, ] <- c(v1, v3, v4)
  faces[6L*(j - 1L) + 3L, ] <- c(v1, v2, v5)
  faces[6L*(j - 1L) + 4L, ] <- c(v2, v3, v5)
  faces[6L*(j - 1L) + 5L, ] <- c(v3, v4, v5)
  faces[6L*(j - 1L) + 6L, ] <- c(v4, v1, v5)
}

# vertices and edges including pyramids ####
Vertices <- rbind(vertices, 1.5 * centers)
edges <- do.call(rbind, lapply(1L:nrow(faces), function(i){
  f <- sort(faces[i, ])
  rbind(
    c(f[1L], f[2L]),
    c(f[1L], f[3L]),
    c(f[2L], f[3L])
  )
}))
edges <- edges[!duplicated(edges), ]

edgeLengths <- apply(edges, 1L, function(ij){
  crossprod(Vertices[ij[1L], ] - Vertices[ij[2L], ])
})
edges <- edges[-which(edgeLengths == 8), ]


# plot ####
s <- 1.8
open3d(windowRect = c(50, 50, 562, 562), zoom = 0.65)
for(i in 1L:nrow(faces)){
  v1 <- Vertices[faces[i, 1L], ]
  v2 <- Vertices[faces[i, 2L], ]
  v3 <- Vertices[faces[i, 3L], ]
  mesh <- gyrotriangle(v1, v2, v3, s = s)
  shade3d(mesh, color = "midnightblue")
}
for(i in 1L:nrow(edges)){
  idx <- edges[i, ]
  A <- Vertices[idx[1L], ]
  B <- Vertices[idx[2L], ]
  edge <- gyrotube(A, B, s = s, radius = 0.05)
  shade3d(edge, color = "yellow")
}
spheres3d(Vertices, radius = 0.06, color = "yellow")
