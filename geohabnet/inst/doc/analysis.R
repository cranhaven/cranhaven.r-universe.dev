## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----eval = FALSE-------------------------------------------------------------
#  if (!require("devtools")) {
#    install.packages("devtools")
#  }

## ----cran, eval=FALSE---------------------------------------------------------
#  
#  if (!require("geohabnet")) {
#    utils::install.packages("geohabnet")
#  }

## ----github-------------------------------------------------------------------
library("devtools")

if (!require("geohabnet")) {
  install_github("GarrettLab/HabitatConnectivity", subdir = "geohabnet")
}

## ----message=FALSE------------------------------------------------------------
library(geohabnet)

## ----message=FALSE------------------------------------------------------------
?geohabnet
# and
?geohabnet::msean # any function

## ----message=FALSE------------------------------------------------------------
res <- sensitivity_analysis()

## ----message=FALSE------------------------------------------------------------

avocado <- cropharvest_rast("avocado", "monfreda")

# verify the raster object
avocado
gplot(avocado)

## ----message=FALSE------------------------------------------------------------
geo_net <- msean(avocado)

## -----------------------------------------------------------------------------
global_scales()

## -----------------------------------------------------------------------------
#set_global_scales(list(east = c(-24, 180, -58, 60), west = c(-140, -34, -58, 60)))

## ----reso---------------------------------------------------------------------
reso()

## -----------------------------------------------------------------------------
config_file <- get_parameters(out_path = tempdir())
config_file

## -----------------------------------------------------------------------------
set_parameters(new_params = config_file)
#using iwindow = true will prompt a selction window to choose config file.

## -----------------------------------------------------------------------------
geohabnet::supported_sources()

## -----------------------------------------------------------------------------
search_crop("avocado") #1
search_crop("bean") #2

## ----fetch_mon_av-------------------------------------------------------------
# get avocado data
rast_avocado <- crops_rast(list(monfreda = "avocado"))

## ----fetch_sp_ba--------------------------------------------------------------

old_to <- options("timeout")
options(timeout = 6000)

#get data of banana crop
rast_ban <- crops_rast(list(mapspam2010 = "banana"))

## ----fetch_multi--------------------------------------------------------------
# Back to back downloads causes connection to fail, this is a workaround
rast_avo_ban <- crops_rast(list(monfreda = c("avocado", "banana"), mapspam2010 = c("banana")))
old_to <- options(old_to)

## ----combine_sps--------------------------------------------------------------
gplot(c(rast_avocado, rast_ban, rast_avo_ban))

## ----run_msean, eval = FALSE, message=FALSE-----------------------------------
#  results <- lapply(list(rast_avocado, rast_avo_ban), msean)

## ----message=FALSE------------------------------------------------------------
results <- sensitivity_analysis()

## ----message=FALSE------------------------------------------------------------
risk_indexes <- msean(avocado, global = FALSE, hd_threshold = 0.00001, link_threshold = 0.00001)

## -----------------------------------------------------------------------------
dist_methods()

## -----------------------------------------------------------------------------
supported_metrics()

## ----non-global, message=FALSE------------------------------------------------
results <- msean(avocado, global = FALSE, geoscale = c(-115, -75, 5, 32))

## ----final--------------------------------------------------------------------
final <- msean(avocado, link_threshold = 0.000001, hd_threshold = 0.000025)

# checkout the type of an object
class(final)

## ----see----------------------------------------------------------------------
final

## ----mean---------------------------------------------------------------------
gplot(final@me_rast)

## ----var----------------------------------------------------------------------
gplot(final@var_rast)

## ----diff---------------------------------------------------------------------
gplot(final@diff_rast)

## ----geonet-------------------------------------------------------------------

# checkout the results
final@rasters

# global is TRUE because we original set the global analysis
# thus, we will have set of 2 risk indices, eastern and wetern hemisphere
final@rasters$global_rast

# Number of elements from above determines the the number of parameter values provided

# To access the adjacency matrix,
final@rasters$global_rast[[1]]$east[[1]] # this is also s4 class 'GeoModel'

## ----adm----------------------------------------------------------------------

# replace the indexing with any arbitary index,
# uncomment line below to see the results.

#final@rasters$global_rast[[1]]$east[[1]]$amatrix

## ----fut, eval = FALSE--------------------------------------------------------
#  avocado <- geohabnet::cropharvest_rast("avocado", "monfreda")
#  
#  # see ?future::plan for details
#  future::plan(future::multicore())
#  msean(avocado)
#  
#  future::plan(future::multisession())
#  msean(avocado)

