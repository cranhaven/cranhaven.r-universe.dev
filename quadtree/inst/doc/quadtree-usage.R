## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.height = 4.375,
  fig.width = 7,
  dev = "jpeg"
)
old_par <- par(no.readonly = TRUE)

## ----message = FALSE----------------------------------------------------------
library(terra)
library(quadtree)

habitat <- terra::rast(system.file("extdata", "habitat.tif", package="quadtree"))
rast <- habitat

qt <- quadtree(rast, .15)

## -----------------------------------------------------------------------------
qt

## -----------------------------------------------------------------------------
projection(qt)

## -----------------------------------------------------------------------------
n_cells(qt)
n_cells(qt, terminal_only = FALSE)

## -----------------------------------------------------------------------------
quadtree::extent(qt)
quadtree::extent(qt, original = TRUE)

## -----------------------------------------------------------------------------
pts <- cbind(x = c(5609, 3959, 20161, 27662, 32763),
             y = c(10835, 29586, 31836, 10834, 36337))

plot(qt, crop = TRUE, border_lwd = .3, na_col = NULL)
points(pts, pch = 16)

## -----------------------------------------------------------------------------
quadtree::extract(qt, pts)
quadtree::extract(qt, pts, extents = TRUE)

## -----------------------------------------------------------------------------
get_neighbors(qt, as.numeric(pts[3,]))

## -----------------------------------------------------------------------------
qt_copy <- copy(qt)
qt
qt_copy

## -----------------------------------------------------------------------------
qt2 <- copy(qt)
set_values(qt2, pts, rep(2, nrow(pts)))
plot(qt2, crop = TRUE, border_lwd = .3)

## -----------------------------------------------------------------------------
qt3 <- copy(qt)
transform_values(qt3, function(x) x^3)

par(mfrow = c(1,2))
plot(qt, crop = TRUE, na_col = NULL, border_lwd = .3, zlim = c(0, 1),
     legend = FALSE, main = "original quadtree")
plot(qt3, crop = TRUE, na_col = NULL, border_lwd = .3, zlim = c(0, 1),
     legend = FALSE, main = "values cubed")

## ---- error = TRUE------------------------------------------------------------
qt_temp <- copy(qt)
filepath <- tempfile()
save(qt_temp, file = filepath)
load(filepath)
qt_temp

## -----------------------------------------------------------------------------
filepath <- tempfile()
write_quadtree(filepath, qt)
qt_read <- read_quadtree(filepath)
qt_read

## -----------------------------------------------------------------------------
vec1 <- as_vector(qt)
length(vec1)
summary(vec1)

vec2 <- as_vector(qt, FALSE)
length(vec2)
summary(vec2)

## -----------------------------------------------------------------------------
df1 <- as_data_frame(qt)
dim(df1)
head(df1)

df2 <- as_data_frame(qt, FALSE)
dim(df2)
head(df2)

## -----------------------------------------------------------------------------
rast <- as_raster(qt)
plot(rast)

## ---- echo=FALSE--------------------------------------------------------------
par(old_par)

