## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
fig.dim = c(6, 4)
)

## ----setup, message=FALSE, warning=FALSE--------------------------------------
library(phylospatial); library(tmap)
ps <- moss()

## ----alpha, message=FALSE, warning=FALSE--------------------------------------
div <- ps_diversity(ps, metric = c("PD", "PE"))
tm_shape(div$PD) + 
      tm_raster(col.scale = tm_scale_continuous(values = "inferno")) +
      tm_layout(legend.outside = TRUE)
tm_shape(div$PE) + 
      tm_raster(col.scale = tm_scale_continuous(values = "inferno")) +
      tm_layout(legend.outside = TRUE)

## ----rand, eval=FALSE---------------------------------------------------------
# rand <- ps_rand(ps, n_rand = 1000, progress = FALSE,
#                 metric = c("PD", "PE", "CE", "RPE"))
# tm_shape(rand$qPE) +
#       tm_raster(col.scale = tm_scale_continuous(values = "inferno")) +
#       tm_layout(legend.outside = TRUE)

## ----precompute, eval=FALSE, echo=FALSE---------------------------------------
# # pre-process to avoid exceeding CRAN runtime limits -- need to manually run this when updating vignette!
# rand0 <- ps_rand(ps, n_rand = 1000, metric = c("PD", "PE", "CE", "RPE"))
# terra::writeRaster(rand0, "~/Documents/R/phylospatial/inst/extdata/alpha-diversity-rand.tif", overwrite = TRUE)

## ----postcompute, echo=FALSE--------------------------------------------------
rand <- terra::rast(system.file("extdata", "alpha-diversity-rand.tif", package = "phylospatial"))
tm_shape(rand$qPE) + 
      tm_raster(col.scale = tm_scale_continuous(values = "inferno")) +
      tm_layout(legend.outside = TRUE)

## ----rand2, message=FALSE, warning=FALSE, eval=FALSE--------------------------
# ps2 <- ps_simulate(data_type = "abundance")
# rand2 <- ps_rand(ps2, fun = "nullmodel", method = "abuswap_c", progress = FALSE, metric = "PD")

## ----canape, message=FALSE, warning=FALSE-------------------------------------
cp <- ps_canape(rand, alpha = .05)
terra::plot(cp)

