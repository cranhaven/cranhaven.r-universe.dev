## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(slopes)

## ----cumulative-slopes, fig.cap="Illustration of the importance of slope length. 4 segments with an 8% gradient is not the same as a single segment with a gradient of 8%.", out.width="40%", echo=FALSE----
knitr::include_graphics("SLOPES-commulative-slope-1.png")

## -----------------------------------------------------------------------------
x = c(0, 2, 3, 4, 5, 9)
y = c(0, 0, 0, 0, 0, 9)
z = c(1, 2, 2, 4, 3, 1) / 10
m = cbind(x, y, z)
d = sequential_dist(m = m, lonlat = FALSE)

slopes::slope_distance_weighted(d = d, elevations = z)
slopes::slope_distance_mean(d = d, elevations = z)

## ----weighted, fig.cap="Illustration of example data that demonstrates distance-weighted mean gradient, used by default in the slopes package."----
plot(x, z, ylim = c(-0.5, 0.5), type = "l")
(gxy = slope_matrix(m, lonlat = FALSE))
abline(h = 0, lty = 2)
points(x[-length(x)], gxy, col = "blue")
title("Distance elevation profile",
  sub = "Points show calculated gradients of subsequent lines")

