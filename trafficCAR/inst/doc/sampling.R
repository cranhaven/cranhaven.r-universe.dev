## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  message = FALSE,
  warning = FALSE,
  fig.width = 6,
  fig.height = 4,
  dpi = 72,
  fig.retina = 1
)

## -----------------------------------------------------------------------------
library(trafficCAR)
library(Matrix)

## -----------------------------------------------------------------------------
A_small <- matrix(
  c(0, 1, 0,
    1, 0, 1,
    0, 1, 0),
  nrow = 3,
  byrow = TRUE
)
A_small

## -----------------------------------------------------------------------------
Q_car <- car_precision(A_small, type = "proper", rho = 0.5, tau = 1)
Q_car

## -----------------------------------------------------------------------------
Q_icar <- car_precision(A_small, type = "icar", tau = 1)
Q_icar

## -----------------------------------------------------------------------------
x0 <- trafficCAR:::rmvnorm_prec(Q_car)
x0

## -----------------------------------------------------------------------------
b <- c(1, 0, -1)
x1 <- trafficCAR:::rmvnorm_prec(Q_car, b = b)
x1

## -----------------------------------------------------------------------------
# ICAR precision matrices are singular, so direct Cholesky-based sampling may fail.
# A practical approach for simulation is to add a small ridge and then center.
# This yields an approximate draw on the ICAR subspace.

n_icar <- nrow(Q_icar)
eps <- 1e-6
Q_icar_ridge <- Q_icar + Matrix::Diagonal(n_icar, x = eps)

x_icar <- trafficCAR:::rmvnorm_prec(Q_icar_ridge)
x_icar_centered <- x_icar - mean(x_icar)
x_icar_centered

## -----------------------------------------------------------------------------
A_road <- NULL

has_sf <- requireNamespace("sf", quietly = TRUE)

if (has_sf) {
  data("roads_small", package = "trafficCAR")
  roads <- roads_small

  segments <- roads_to_segments(
    roads,
    crs_m = 3857,
    split_at_intersections = TRUE
  )

  if (nrow(segments) > 200) {
    segments <- segments[seq_len(200), ]
  }

  adjacency <- build_adjacency(segments, crs_m = 3857)

  # Drop isolated segments to keep CAR models well-defined.
  if (any(adjacency$isolates)) {
    segments <- segments[!adjacency$isolates, ]
    adjacency <- build_adjacency(segments, crs_m = 3857)
  }

  A_road <- adjacency$A
}

## -----------------------------------------------------------------------------
if (is.null(A_road)) {
  # nodes: 1--2--3 and 2--4 (T-junction at node 2)
  edges <- matrix(c(
    1, 2,
    2, 3,
    2, 4
  ), ncol = 2, byrow = TRUE)

  if (requireNamespace("igraph", quietly = TRUE)) {
    g <- igraph::graph_from_edgelist(edges, directed = FALSE)
    A_road <- igraph::as_adjacency_matrix(g, sparse = TRUE, attr = NULL)
  } else {
    # minimal fallback without igraph (dense, small only)
    n <- max(edges)
    A_road <- matrix(0, n, n)
    for (k in seq_len(nrow(edges))) {
      i <- edges[k, 1]; j <- edges[k, 2]
      A_road[i, j] <- 1
      A_road[j, i] <- 1
    }
    A_road <- Matrix(A_road, sparse = TRUE)
  }
}

c(n = nrow(A_road), nnz = Matrix::nnzero(A_road))

## -----------------------------------------------------------------------------
# proper CAR (users should choose rho consistent with their graph and model)
Q_car_road <- car_precision(A_road, type = "proper", rho = 0.5, tau = 1)

# ICAR
Q_icar_road <- car_precision(A_road, type = "icar", tau = 1)

x_car_road <- trafficCAR:::rmvnorm_prec(Q_car_road)

n_road <- nrow(Q_icar_road)
eps <- 1e-6
Q_icar_road_ridge <- Q_icar_road + Matrix::Diagonal(n_road, x = eps)
x_icar_road <- trafficCAR:::rmvnorm_prec(Q_icar_road_ridge)
x_icar_road <- x_icar_road - mean(x_icar_road)

summary(x_car_road)
summary(x_icar_road)

