## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  fig.align = "center",
  fig.width = 3.5,
  fig.height = 3,
  comment = "#>"
)

## -----------------------------------------------------------------------------
library(geostan)
library(sf)

## -----------------------------------------------------------------------------
# create a regular grid
row = col = 3
sfc = st_sfc(st_polygon(list(rbind(c(0,0), c(col,0), c(col,row), c(0,0)))))
grid <- st_make_grid(sfc, cellsize = 1, square = TRUE)
grid <- st_as_sf(grid)

ogpar <- par(mar = rep(0, 4))
plot( grid )
par(ogpar)

## -----------------------------------------------------------------------------
A <- shape2mat(grid, method = 'rook')

## -----------------------------------------------------------------------------
n_nbs(A)
summary( n_nbs(A) )

## -----------------------------------------------------------------------------
Aq <- shape2mat(grid, method = 'queen')
n_nbs(Aq)

## -----------------------------------------------------------------------------
# geometry of the grid
geom <- st_geometry(grid)

# geometry of the graph
edges <- edges(A, shape = grid)
graph <- st_geometry(edges)

# plot overlay
ogpar <- par(mar = rep(0, 4))
plot(geom, lwd = .1)
plot(graph, add = TRUE, type = 'b')
par(ogpar)

## -----------------------------------------------------------------------------
Aq <- shape2mat(grid, method = 'queen')
E <- edges(Aq, shape = grid)
graph <- st_geometry(E)

ogpar <- par(mar = rep(0, 4))
plot(geom, lwd = .1)
plot(graph, add = TRUE, type = 'b')
par(ogpar)

## -----------------------------------------------------------------------------
A4 <- shape2mat(grid, method = 'knn', k = 4)
n_nbs(A4)

## -----------------------------------------------------------------------------
W <- row_standardize(A)
print( W )

## -----------------------------------------------------------------------------
W <- shape2mat(grid, 'W', method = 'rook')
print( W )

## -----------------------------------------------------------------------------
# row number, index position 
Id <- 1:nrow(grid)

# centroid coordinates (x, y)
centers <- st_centroid(grid)
coords <- st_geometry(centers)
xy <- matrix(unlist(coords), byrow = T, ncol = 2)

# map Ids
ogpar <- par(mar = rep(0, 4))
plot(geom, lwd = .15)
text(x = xy[,1], y = xy[,2],
     label = Id)
par(ogpar)     

## -----------------------------------------------------------------------------
A[2, 4] <- A[4, 2] <- TRUE # 1

## -----------------------------------------------------------------------------
A[1, 2] <- A[2, 1] <- FALSE # 0

## -----------------------------------------------------------------------------
# draw data from spatial autoregressive model
set.seed(101010)
y <- sim_sar(w = W, rho = .8)
print( y )

ogpar <- par(mar = rep(0, 4))
grid$y <- y
plot(grid['y'])
par(ogpar)

## -----------------------------------------------------------------------------
A <- shape2mat(grid, method = 'rook', quiet = TRUE)
w1 = A[1,]
print( w1 )

## -----------------------------------------------------------------------------
w1_y <- w1 * y
sum( w1_y )

## -----------------------------------------------------------------------------
w1 %*% y

## -----------------------------------------------------------------------------
A %*% y

## -----------------------------------------------------------------------------
W %*% y

## -----------------------------------------------------------------------------
dev.off()

