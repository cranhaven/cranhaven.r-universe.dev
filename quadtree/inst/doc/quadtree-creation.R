## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.height = 4.375,
  fig.width = 7,
  dev = "jpeg",
  out.width = "100%"
)
options(rmarkdown.html_vignette.check_title = FALSE)
old_par <- par(no.readonly = TRUE)

## ---- message = FALSE, warning = FALSE----------------------------------------
library(quadtree)
library(terra)

habitat <- terra::rast(system.file("extdata", "habitat.tif", package="quadtree"))
rast <- habitat
plot(rast, main = "sample raster")

## ---- echo=FALSE, out.width = "100%"------------------------------------------
knitr::include_graphics("figures/quadtree-structure-illustration.png")

## -----------------------------------------------------------------------------
dim(rast) # not a power of 2
qt <- quadtree(rast, .15, adj_type = "expand")
plot(qt, border_lwd = .3, main = "expand raster dimensions")

## -----------------------------------------------------------------------------
qt <- quadtree(rast, .15, adj_type = "resample", resample_n_side = 128,
               resample_pad_nas = FALSE)
plot(qt, border_lwd = .3, main = "resample (without NA padding)")

## -----------------------------------------------------------------------------
qt <- quadtree(rast, .15, adj_type = "resample", resample_n_side = 128)
plot(qt, border_lwd = .3, main = "resample (with NA padding)")

## -----------------------------------------------------------------------------
qt <- quadtree(rast, .15, adj_type = "none")
plot(qt, main = "adj_type = 'none'")

## -----------------------------------------------------------------------------
qt_range <- quadtree(rast, .1, split_method = "range")
qt_sd <- quadtree(rast, .1, split_method = "sd")
qt_cv <- quadtree(rast, .1, split_method = "cv")

par(mfrow = c(1, 3), mar = c(0,0,3,0))
plot(qt_range, crop = TRUE, na_col = NULL, zlim = c(0, 1), border_lwd = .3,
     axes = FALSE, legend = FALSE, main = "split_method = 'range'")
plot(qt_sd, crop = TRUE, na_col = NULL, zlim = c(0,1), border_lwd = .3,
     axes = FALSE, legend = FALSE, main = "split_method = 'sd'")
plot(qt_cv, crop = TRUE, na_col = NULL, zlim = c(0,1), border_lwd = .3,
     axes = FALSE, legend = FALSE, main = "split_method = 'cv'")

## -----------------------------------------------------------------------------
qt_mean <- quadtree(rast, .1, "sd", combine_method = "mean")
qt_median <- quadtree(rast, .1, "sd", combine_method = "median")
qt_min <- quadtree(rast, .1, "sd", combine_method = "min")
qt_max <- quadtree(rast, .1, "sd", combine_method = "max")

par(mfrow = c(2, 2), mar = c(.5, .5, .5, .5))
plot(qt_mean, crop = TRUE, na_col = NULL, axes = FALSE, legend = TRUE,
     border_lwd = .3, zlim = c(0,1), main = "mean")
plot(qt_median, crop = TRUE, na_col = NULL, axes = FALSE, legend = TRUE,
     border_lwd = .3, zlim = c(0,1), main = "median")
plot(qt_min, crop = TRUE, na_col = NULL, axes = FALSE, legend = TRUE,
     border_lwd = .3, zlim = c(0,1), main = "min")
plot(qt_max, crop = TRUE, na_col = NULL, axes = FALSE, legend = TRUE,
     border_lwd = .3, zlim = c(0,1), main = "max")

## -----------------------------------------------------------------------------
split_fun <- function(vals, args) {
  return(any(vals < args$threshold))
}

## -----------------------------------------------------------------------------
qt <- quadtree(rast, split_method = "custom", split_fun = split_fun,
                split_args = list(threshold = .8))
plot(qt, crop = TRUE, na_col = NULL, border_lwd = .3,
     main = "custom splitting function")

## -----------------------------------------------------------------------------
cmb_fun <- function(vals, args) {
  if (any(is.na(vals))) {
    return(NA)
  }
  if (mean(vals) < args$threshold) {
    return(0)
  } else {
    return(1)
  }
}

qt <- quadtree(rast, .1, combine_method = "custom", combine_fun = cmb_fun,
               combine_args = list(threshold = .5))
plot(qt, crop = TRUE, na_col = NULL, border_lwd = .3,
     main = "custom combine function")

## -----------------------------------------------------------------------------
habitat_roads <- terra::rast(system.file("extdata", "habitat_roads.tif", package="quadtree"))
template <- habitat_roads

# use a custom function so that a quadrant is split if it contains any 1's
split_if_one <- function(vals, args) {
  if(any(vals == 1, na.rm = TRUE)) return(TRUE)
  return(FALSE)
}
qt_template <- quadtree(template, split_method = "custom",
                        split_fun = split_if_one)

# now use the template to create a quadtree from 'rast'
qt <- quadtree(rast, template_quadtree = qt_template)

par(mfrow = c(1, 3), mar = c(0,0,3,0))
plot(template, axes = FALSE, box = FALSE, legend = FALSE,
     main = "template raster")
plot(qt_template, crop = TRUE, na_col = NULL, border_lwd = .3 ,axes = FALSE,
     legend = FALSE, main = "template quadtree")
plot(qt, crop = TRUE, na_col = NULL, border_lwd = .3, axes = FALSE,
     legend = FALSE, main = "quadtree created using template")

## -----------------------------------------------------------------------------
qt_max_cell <- quadtree(rast, .15, max_cell_length = 1000)
qt_min_cell <- quadtree(rast, .15, min_cell_length = 1000)

par(mfrow = c(1, 2))
plot(qt_max_cell, crop = TRUE, na_col = NULL, border_lwd = .3, legend = FALSE,
     main = "max_cell_length = 1000")
plot(qt_min_cell, crop = TRUE, na_col = NULL, border_lwd = .3, legend = FALSE,
     main = "min_cell_length = 1000")

## -----------------------------------------------------------------------------
qt_any <- quadtree(rast, .15, split_if_any_na = FALSE)
qt_all <- quadtree(rast, .15, split_if_all_na = TRUE)

par(mfrow = c(1, 2))
plot(qt_any, border_lwd = .3, legend = FALSE, main = "split_if_any_na = FALSE")
plot(qt_all, border_lwd = .3, legend = FALSE, main = "split_if_all_na = TRUE")

## ---- echo = FALSE------------------------------------------------------------
par(old_par)

