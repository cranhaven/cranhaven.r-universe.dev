## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  eval = FALSE
)

## ----setup--------------------------------------------------------------------
#   # CRAN limite CPU usage
#  data.table::setDTthreads(2)
#  library(antaresEditObject)

## -----------------------------------------------------------------------------
#  tmp <- tempfile()
#  createStudy(path = tmp, study_name = "res-study")
#  opts <- setSimulationPath(tmp)

## -----------------------------------------------------------------------------
#  activateRES()

## -----------------------------------------------------------------------------
#  updateOptimizationSettings(renewable.generation.modelling = "clusters")

## -----------------------------------------------------------------------------
#  # create an area to create a cluster in it
#  createArea(name = "area51")
#  
#  # Create a renewable cluster
#  createClusterRES(
#    area = "area51",
#    cluster_name = "ren01",
#    add_prefix = FALSE
#  )
#  
#  # Use an other group and some parameters
#  createClusterRES(
#    area = "area51",
#    cluster_name = "ren02",
#    group = "Wind Offshore",
#    nominalcapacity = 123,
#    ts_interpretation = "production-factor",
#    add_prefix = FALSE
#  )

## -----------------------------------------------------------------------------
#  editClusterRES(
#    area = "area51",
#    cluster_name = "ren02",
#    group = "Solar Rooftop", # new group
#    add_prefix = FALSE
#  )

## -----------------------------------------------------------------------------
#  removeClusterRES(area = "area51", cluster_name = "ren01")

## -----------------------------------------------------------------------------
#  readScenarioBuilder()

