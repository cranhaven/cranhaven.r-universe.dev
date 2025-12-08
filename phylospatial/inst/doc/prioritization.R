## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.dim = c(6, 4)
)

## ----init, message=FALSE, warning=FALSE---------------------------------------
library(phylospatial); library(tmap); library(magrittr)

ps <- moss()
init <- seq(1, 0, length.out = nrow(ps$comm))
cost <- runif(nrow(ps$comm), 10, 1000)

## ----optim, eval=FALSE--------------------------------------------------------
# priority <- ps_prioritize(ps, init = init, cost = cost)
# 
# tm_shape(priority) +
#       tm_raster(col.scale = tm_scale_continuous_log(values = "-inferno")) +
#       tm_layout(legend.outside = TRUE)

## ----precompute, eval=FALSE, echo=FALSE---------------------------------------
# # pre-process to avoid exceeding CRAN runtime limits -- need to manually run this when updating vignette!
# priority <- ps_prioritize(ps, init = init, cost = cost, progress = FALSE)
# terra::writeRaster(priority, "~/Documents/R/phylospatial/inst/extdata/priority.tif", overwrite = TRUE)

## ----postcompute, echo=FALSE--------------------------------------------------
priority <- terra::rast(system.file("extdata", "priority.tif", package = "phylospatial"))

tm_shape(priority) + 
      tm_raster(col.scale = tm_scale_continuous_log(values = "-inferno")) + 
      tm_layout(legend.outside = TRUE)

## ----prob, eval=FALSE---------------------------------------------------------
# priority <- ps_prioritize(ps, init = init, cost = cost, n_reps = 2500,
#                           method = "prob", max_iter = 10)
# 
# tm_shape(priority$top10) +
#       tm_raster(col.scale = tm_scale_continuous(values = "inferno"),
#                 col.legend = tm_legend(title = "proporiton of runs\nin which site was\ntop-10 priority")) +
#       tm_layout(legend.outside = TRUE)

## ----precompute2, eval=FALSE, echo=FALSE--------------------------------------
# # pre-process to avoid exceeding CRAN runtime limits -- need to manually run this when updating vignette!
# priority <- ps_prioritize(ps, init = init, cost = cost, n_reps = 2500,
#                           method = "prob", max_iter = 10)
# terra::writeRaster(priority, "~/Documents/R/phylospatial/inst/extdata/priority-prob.tif", overwrite = TRUE)

## ----postcompute2, echo=FALSE-------------------------------------------------
priority <- terra::rast(system.file("extdata", "priority-prob.tif", package = "phylospatial"))

tm_shape(priority$top10) + 
      tm_raster(col.scale = tm_scale_continuous(values = "inferno"),
                col.legend = tm_legend(title = "proporiton of runs\nin which site was\ntop-10 priority")) + 
      tm_layout(legend.outside = TRUE)

## ----lambda, fig.dim = c(4.5, 5)----------------------------------------------
plot_lambda()

