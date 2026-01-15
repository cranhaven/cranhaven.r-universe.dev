## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(RCDT)

## ----pentagram, fig.width=6, fig.height=6-------------------------------------
# vertices
R <- sqrt((5-sqrt(5))/10)     # outer radius
r <- sqrt((25-11*sqrt(5))/10) # circumradius of the inner pentagon
k <- pi/180 # factor to convert degrees to radians
X <- R * vapply(0L:4L, function(i) cos(k * (90+72*i)), numeric(1L))
Y <- R * vapply(0L:4L, function(i) sin(k * (90+72*i)), numeric(1L))
x <- r * vapply(0L:4L, function(i) cos(k * (126+72*i)), numeric(1L))
y <- r * vapply(0L:4L, function(i) sin(k * (126+72*i)), numeric(1L))
vertices <- rbind(
  c(X[1L], Y[1L]),
  c(x[1L], y[1L]),
  c(X[2L], Y[2L]),
  c(x[2L], y[2L]),
  c(X[3L], Y[3L]),
  c(x[3L], y[3L]),
  c(X[4L], Y[4L]),
  c(x[4L], y[4L]),
  c(X[5L], Y[5L]),
  c(x[5L], y[5L])
)
# constraint edges: indices
edges <- cbind(1L:10L, c(2L:10L, 1L))
# constrained Delaunay triangulation
del <- delaunay(vertices, edges)
# plot
opar <- par(mar = c(0, 0, 0, 0))
plotDelaunay(
  del, type = "n", asp = 1, fillcolor = "distinct", lwd_borders = 3,
  xlab = NA, ylab = NA, axes = FALSE
)
par(opar)

## ----pentagram_area-----------------------------------------------------------
delaunayArea(del)
sqrt(650 - 290*sqrt(5)) / 4 # exact value

## ----donut-hexagon, fig.width=6, fig.height=6---------------------------------
nsides <- 6L
angles <- seq(0, 2*pi, length.out = nsides+1L)[-1L]
outer_points <- cbind(cos(angles), sin(angles))
inner_points <- outer_points / 2 
points <- rbind(outer_points, inner_points)
# constraint edges
indices <- 1L:nsides
edges_outer <- cbind(
  indices, c(indices[-1L], indices[1L])
)
edges_inner <- edges_outer + nsides
edges <- rbind(edges_outer, edges_inner)
# constrained Delaunay triangulation
del <- delaunay(points, edges) 
# plot
opar <- par(mar = c(0, 0, 0, 0))
plotDelaunay(
  del, type = "n", asp = 1, axes = FALSE, xlab = NA, ylab = NA, 
  fillcolor = "yellow", lwd_borders = 3, col_borders = "navy"
)
par(opar)

## ----area_donut-hexagon-------------------------------------------------------
delaunayArea(del)
3*sqrt(3)/2 - 3*sqrt(3)/8

