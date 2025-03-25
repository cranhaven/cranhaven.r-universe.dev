library(gyro)
library(rgl)
library(Rvcg)
library(Morpho)
library(arrangements)
library(cxhull)

##~~ Gircope - cubes only ~~##

x <- c(
  (1 + 2*sqrt(2)) / 2,
  (1 + sqrt(2)) / 2,
  1/2
)
perms <- permutations(3L)
vs0 <- cbind(t(apply(perms, 1L, function(perm) x[perm])), 1/2)
vertices <- changesOfSign(vs0)
M <- c(crossprod(vertices[1L, ]))

# hull ####
hull <- cxhull(vertices)
edges <- hull[["edges"]]

# cubic cells
cubicCells <-
  Filter(function(f) length(f[["vertices"]]) == 8L, hull[["facets"]])
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
goodRidges <- do.call(c, lapply(cubicCells, `[[`, "ridges"))
ridges <- hull[["ridges"]]
squares <- t(vapply(
  goodRidges,
  function(r) polygonize(ridges[[r]][["edges"]]),
  integer(4L)
))

# stereographic projection of vertices ####
pvertices <- t(apply(vertices, 1L, function(v){
  v[1L:3L] / (sqrt(M) - v[4L])
}))


# draw ####
s <- 0.5
open3d(windowRect = c(50, 50, 562, 562), zoom = 0.8)
bg3d(rgb(54, 57, 64, maxColorValue = 255))
for(i in 1L:nrow(squares)){
  square <- squares[i, ]
  mesh1 <- gyrotriangle(
    pvertices[square[1L], ], pvertices[square[2L], ], pvertices[square[3L], ],
    s = s
  )
  mesh2 <- gyrotriangle(
    pvertices[square[1L], ], pvertices[square[3L], ], pvertices[square[4L], ],
    s = s
  )
  mesh <- vcgClean(mergeMeshes(mesh1, mesh2), sel = c(0, 7), silent = TRUE)
  shade3d(mesh, color = "violetred")
}
for(i in 1L:nrow(edges)){
  idx <- edges[i, ]
  A <- pvertices[idx[1L], ]; B <- pvertices[idx[2L], ]
  tube <- gyrotube(A, B, s = s, radius = 0.025)
  shade3d(tube, color = "whitesmoke")
}
spheres3d(pvertices, radius = 0.03, color = "whitesmoke")
