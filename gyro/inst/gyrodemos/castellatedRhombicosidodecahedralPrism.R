library(gyro)
library(rgl)
library(cxhull)

##~~ Castellated rhombicosidodecahedral prism - tetrahedra only ~~##

phi <- (1 + sqrt(5)) / 2
phi2 <- phi * phi
phi3 <- phi * phi2
vertices <- rbind(
  c(0, -phi, -phi3, 0),
  c(0, phi, -phi3, 0),
  c(0, -phi, phi3, 0),
  c(0, phi, phi3, 0),
  c(-phi, -phi3, 0, 0),
  c(phi, -phi3, 0, 0),
  c(-phi, phi3, 0, 0),
  c(phi, phi3, 0, 0),
  c(-phi3, 0, -phi, 0),
  c(phi3, 0, -phi, 0),
  c(-phi3, 0, phi, 0),
  c(phi3, 0, phi, 0),
  c(0, -phi3, -phi2, -1),
  c(0, phi3, -phi2, -1),
  c(0, -phi3, phi2, -1),
  c(0, phi3, phi2, -1),
  c(0, -phi3, -phi2, 1),
  c(0, phi3, -phi2, 1),
  c(0, -phi3, phi2, 1),
  c(0, phi3, phi2, 1),
  c(-phi2, 0, -phi3, -1),
  c(phi2, 0, -phi3, -1),
  c(-phi2, 0, phi3, -1),
  c(phi2, 0, phi3, -1),
  c(-phi2, 0, -phi3, 1),
  c(phi2, 0, -phi3, 1),
  c(-phi2, 0, phi3, 1),
  c(phi2, 0, phi3, 1),
  c(-phi3, -phi2, 0, -1),
  c(phi3, -phi2, 0, -1),
  c(-phi3, phi2, 0, -1),
  c(phi3, phi2, 0, -1),
  c(-phi3, -phi2, 0, 1),
  c(phi3, -phi2, 0, 1),
  c(-phi3, phi2, 0, 1),
  c(phi3, phi2, 0, 1),
  c(0, -phi2, -phi-2, -phi),
  c(0, phi2, -phi-2, -phi),
  c(0, -phi2, phi+2, -phi),
  c(0, phi2, phi+2, -phi),
  c(0, -phi2, -phi-2, phi),
  c(0, phi2, -phi-2, phi),
  c(0, -phi2, phi+2, phi),
  c(0, phi2, phi+2, phi),
  c(-phi2, -phi-2, 0, -phi),
  c(phi2, -phi-2, 0, -phi),
  c(-phi2, phi+2, 0, -phi),
  c(phi2, phi+2, 0, -phi),
  c(-phi2, -phi-2, 0, phi),
  c(phi2, -phi-2, 0, phi),
  c(-phi2, phi+2, 0, phi),
  c(phi2, phi+2, 0, phi),
  c(-phi-2, 0, -phi2, -phi),
  c(phi+2, 0, -phi2, -phi),
  c(-phi-2, 0, phi2, -phi),
  c(phi+2, 0, phi2, -phi),
  c(-phi-2, 0, -phi2, phi),
  c(phi+2, 0, -phi2, phi),
  c(-phi-2, 0, phi2, phi),
  c(phi+2, 0, phi2, phi),
  c(-phi2, -phi2, -phi2, 0),
  c(phi2, -phi2, -phi2, 0),
  c(-phi2, phi2, -phi2, 0),
  c(phi2, phi2, -phi2, 0),
  c(-phi2, -phi2, phi2, 0),
  c(phi2, -phi2, phi2, 0),
  c(-phi2, phi2, phi2, 0),
  c(phi2, phi2, phi2, 0),
  c(-1, -1, -phi3, -phi),
  c(1, -1, -phi3, -phi),
  c(-1, 1, -phi3, -phi),
  c(1, 1, -phi3, -phi),
  c(-1, -1, phi3, -phi),
  c(1, -1, phi3, -phi),
  c(-1, 1, phi3, -phi),
  c(1, 1, phi3, -phi),
  c(-1, -1, -phi3, phi),
  c(1, -1, -phi3, phi),
  c(-1, 1, -phi3, phi),
  c(1, 1, -phi3, phi),
  c(-1, -1, phi3, phi),
  c(1, -1, phi3, phi),
  c(-1, 1, phi3, phi),
  c(1, 1, phi3, phi),
  c(-1, -phi3, -1, -phi),
  c(1, -phi3, -1, -phi),
  c(-1, phi3, -1, -phi),
  c(1, phi3, -1, -phi),
  c(-1, -phi3, 1, -phi),
  c(1, -phi3, 1, -phi),
  c(-1, phi3, 1, -phi),
  c(1, phi3, 1, -phi),
  c(-1, -phi3, -1, phi),
  c(1, -phi3, -1, phi),
  c(-1, phi3, -1, phi),
  c(1, phi3, -1, phi),
  c(-1, -phi3, 1, phi),
  c(1, -phi3, 1, phi),
  c(-1, phi3, 1, phi),
  c(1, phi3, 1, phi),
  c(-phi3, -1, -1, -phi),
  c(phi3, -1, -1, -phi),
  c(-phi3, 1, -1, -phi),
  c(phi3, 1, -1, -phi),
  c(-phi3, -1, 1, -phi),
  c(phi3, -1, 1, -phi),
  c(-phi3, 1, 1, -phi),
  c(phi3, 1, 1, -phi),
  c(-phi3, -1, -1, phi),
  c(phi3, -1, -1, phi),
  c(-phi3, 1, -1, phi),
  c(phi3, 1, -1, phi),
  c(-phi3, -1, 1, phi),
  c(phi3, -1, 1, phi),
  c(-phi3, 1, 1, phi),
  c(phi3, 1, 1, phi),
  c(-phi, -2*phi, -phi2, -phi),
  c(phi, -2*phi, -phi2, -phi),
  c(-phi, 2*phi, -phi2, -phi),
  c(phi, 2*phi, -phi2, -phi),
  c(-phi, -2*phi, phi2, -phi),
  c(phi, -2*phi, phi2, -phi),
  c(-phi, 2*phi, phi2, -phi),
  c(phi, 2*phi, phi2, -phi),
  c(-phi, -2*phi, -phi2, phi),
  c(phi, -2*phi, -phi2, phi),
  c(-phi, 2*phi, -phi2, phi),
  c(phi, 2*phi, -phi2, phi),
  c(-phi, -2*phi, phi2, phi),
  c(phi, -2*phi, phi2, phi),
  c(-phi, 2*phi, phi2, phi),
  c(phi, 2*phi, phi2, phi),
  c(-phi2, -phi, -2*phi, -phi),
  c(phi2, -phi, -2*phi, -phi),
  c(-phi2, phi, -2*phi, -phi),
  c(phi2, phi, -2*phi, -phi),
  c(-phi2, -phi, 2*phi, -phi),
  c(phi2, -phi, 2*phi, -phi),
  c(-phi2, phi, 2*phi, -phi),
  c(phi2, phi, 2*phi, -phi),
  c(-phi2, -phi, -2*phi, phi),
  c(phi2, -phi, -2*phi, phi),
  c(-phi2, phi, -2*phi, phi),
  c(phi2, phi, -2*phi, phi),
  c(-phi2, -phi, 2*phi, phi),
  c(phi2, -phi, 2*phi, phi),
  c(-phi2, phi, 2*phi, phi),
  c(phi2, phi, 2*phi, phi),
  c(-2*phi, -phi2, -phi, -phi),
  c(2*phi, -phi2, -phi, -phi),
  c(-2*phi, phi2, -phi, -phi),
  c(2*phi, phi2, -phi, -phi),
  c(-2*phi, -phi2, phi, -phi),
  c(2*phi, -phi2, phi, -phi),
  c(-2*phi, phi2, phi, -phi),
  c(2*phi, phi2, phi, -phi),
  c(-2*phi, -phi2, -phi, phi),
  c(2*phi, -phi2, -phi, phi),
  c(-2*phi, phi2, -phi, phi),
  c(2*phi, phi2, -phi, phi),
  c(-2*phi, -phi2, phi, phi),
  c(2*phi, -phi2, phi, phi),
  c(-2*phi, phi2, phi, phi),
  c(2*phi, phi2, phi, phi)
)


# hull ####
hull <- cxhull(vertices)
edges <- hull[["edges"]]

# tetrahedral cells
tetrahedralCells <-
  Filter(function(f) length(f[["vertices"]]) == 4L, hull[["facets"]])

goodRidges <- do.call(c, lapply(tetrahedralCells, `[[`, "ridges"))
ridges <- hull[["ridges"]]
triangles <- t(vapply(
  goodRidges,
  function(r) unique(c(ridges[[r]][["edges"]])),
  integer(3L)
))

# stereographic-like projection of vertices ####
M <- sqrt(max(vapply(1L:nrow(vertices), function(i){
  crossprod(vertices[i, ])
}, numeric(1L))))
pvertices <- t(apply(vertices, 1L, function(v){
  sgn <- sign(v[4L])
  acos(v[4L]/M) / (M^(4/5)-(sgn*v[4L])^(4/5))^(5/4) * v[1L:3L]
}))


# draw ####
s <- 0.95
open3d(windowRect = c(50, 50, 562, 562), zoom = 0.7)
bg3d(rgb(54, 57, 64, maxColorValue = 255))
for(i in 1L:nrow(triangles)){
  trgl <- triangles[i, ]
  mesh <- gyrotriangle(
    pvertices[trgl[1L], ], pvertices[trgl[2L], ], pvertices[trgl[3L], ],
    s = s
  )
  shade3d(mesh, color = "violetred")
}
for(i in 1L:nrow(edges)){
  idx <- edges[i, ]
  A <- pvertices[idx[1L], ]; B <- pvertices[idx[2L], ]
  tube <- gyrotube(A, B, s = s, radius = 0.015)
  shade3d(tube, color = "whitesmoke")
}
spheres3d(pvertices, radius = 0.025, color = "whitesmoke")
