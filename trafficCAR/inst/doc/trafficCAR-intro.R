## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
library(trafficCAR)
library(Matrix)

data("roads_small", package = "trafficCAR")
roads_sf <- roads_small

## -----------------------------------------------------------------------------
net <- build_network(
  roads_sf,
  crs_out = 3857,
  node_intersections = TRUE,
  snap_tol = 0,
  simplify = TRUE
)

c(
  nodes = nrow(net$nodes),
  edges = nrow(net$edges),
  adjacency_nnz = Matrix::nnzero(net$A)
)

## -----------------------------------------------------------------------------
W_row <- weights_from_adjacency(net$A, style = "row-standardized")
Matrix::rowSums(W_row)[1:6]

## -----------------------------------------------------------------------------
Q_car <- car_precision(net$A, type = "proper", rho = 0.25, tau = 1)

Q_icar <- intrinsic_car_precision(net$A, tau = 1, scale = FALSE)

Q_car[1:6, 1:6]
dim(Q_icar)

## -----------------------------------------------------------------------------
set.seed(123)
theta <- trafficCAR:::rmvnorm_prec(Q_car)
c(mean = mean(theta), sd = sd(theta))

## -----------------------------------------------------------------------------
roads_sf <- roads_small

# mock traffic quantities (stand-in for augment_roads() output)
set.seed(123)

# many mapping helpers expect these standard columns
roads_sf$predicted_mean <- runif(nrow(roads_sf), min = 20, max = 60)
roads_sf$relative_congestion <- as.numeric(scale(runif(nrow(roads_sf))))

has_leaflet <- requireNamespace("leaflet", quietly = TRUE) &&
  requireNamespace("viridisLite", quietly = TRUE)

if (has_leaflet) {
  map_roads_interactive(roads_sf, value = "predicted_speed")
} else {
  message("Install 'leaflet' and 'viridisLite' to view the interactive map.")
}

## -----------------------------------------------------------------------------
if (has_leaflet) {
  map_roads_interactive_layers(
    roads_sf,
    values = c("predicted_speed", "relative_congestion")
  )
}

