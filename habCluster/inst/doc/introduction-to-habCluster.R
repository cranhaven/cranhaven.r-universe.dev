## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----example, message=FALSE, warning=FALSE, include=FALSE, paged.print=TRUE----
library(sf)
library(stars)
library(habCluster)

## ----read_data----------------------------------------------------------------
hsi.file = system.file("extdata","wolf3_int.tif",package="habCluster")
wolf = read_stars(hsi.file)
# rescale raster value to 0 - 1
wolf = wolf / 100

## ----compute------------------------------------------------------------------
clst = cluster(wolf, method = cluster_leiden, cellsize = 40000, resolution_parameter = 0.0002, silent = FALSE)

## ----cluster, echo=TRUE, fig.height=5, fig.width=5----------------------------
image(wolf, col = terrain.colors(100,rev = T), asp = 1)
boundary = clst$boundary
plot( boundary$geometry, add=TRUE, asp=1, border = "lightseagreen")

## ----discard, echo=TRUE, fig.height=5, fig.width=5----------------------------
image(wolf, col = terrain.colors(100,rev = T), asp = 1)
boundary$area = as.numeric(st_area(boundary))
boundary = boundary[boundary$area > 40000*40000,]
plot( boundary$geometry, add=TRUE, asp=1, border = "lightseagreen")

## ----Louvain, fig.height=5, fig.width=5---------------------------------------
clst = cluster(wolf, method = cluster_fast_greedy, cellsize = 40000)
image(wolf, col = terrain.colors(100,rev = T), asp = 1)
boundary = clst$boundary
plot( boundary$geometry, add=TRUE, asp=1, border = "lightseagreen")

## ----raster-------------------------------------------------------------------
# library(raster)
# wolf = raster(hsi.file)
# wolf = wolf / 100
# clst = cluster(wolf, method = cluster_leiden, cellsize = 40000, rp = 0.0002, silent = FALSE)

