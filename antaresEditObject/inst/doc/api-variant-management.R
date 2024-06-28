## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  eval = TRUE
)

## ----setup--------------------------------------------------------------------
 # CRAN limite CPU usage
data.table::setDTthreads(2)
library(antaresEditObject)

## ----set-simulation-path, eval=FALSE------------------------------------------
#  antaresRead::setSimulationPathAPI(
#    host = "http://localhost:8080",
#    study_id = "70a08fae-da67-444a-b2ed-df4c0f956a31",
#    token = NULL,
#    simulation = "input"
#  )

## ----create-variant, eval=FALSE-----------------------------------------------
#  # Create new variant
#  createVariant("variant-1")
#  
#  # use an existing one
#  useVariant("variant-2")

## ----mock-api-----------------------------------------------------------------
mockSimulationAPI()

## ----set-api-mode, eval=FALSE-------------------------------------------------
#  setAPImode("async")
#  # or
#  setAPImode("sync")

## ----get-api-commands, eval=FALSE---------------------------------------------
#  getVariantCommands()

## ----get-api-commands-last, eval=FALSE----------------------------------------
#  getVariantCommands(last = TRUE)
#  # or use a numeric to get the last N commands
#  getVariantCommands(last = 3)

## ----get-api-commands-actions, eval=FALSE-------------------------------------
#  getVariantCommands(actions = "create_area")

## ----write-api-commands, eval=FALSE-------------------------------------------
#  writeVariantCommands("path/to/commands.json")

## ----echo=FALSE---------------------------------------------------------------
knitr::include_graphics("figures/badge_api_ok.svg")

## ----create_area--------------------------------------------------------------
createArea(name = "area01")
createArea(name = "area02")
createArea(name = "area03")
getVariantCommands()

## ----create_area2-------------------------------------------------------------
createArea(
  name = "area04", 
  filtering = filteringOptions(filter_synthesis = c("hourly", "daily"))
)
getVariantCommands()

## ----remove_area--------------------------------------------------------------
createArea(name = "area000")
# editArea(name = "area000", ...)
removeArea(name = "area000")
getVariantCommands(last = TRUE)

## ----create_link--------------------------------------------------------------
createLink(from = "area01", to = "area02")
createLink(from = "area01", to = "area03")
getVariantCommands(last = 2)

## ----edit_link----------------------------------------------------------------
editLink(
  from = "area01", 
  to = "area02",
  dataLink = matrix(data = c(rep(9, 8760*2), rep(6, 8760*6)), ncol = 8)
)
getVariantCommands(last = 2)

## ----remove_link--------------------------------------------------------------
removeLink(from = "area01", to = "area03")
getVariantCommands(last = TRUE)

## ----create_cluster-----------------------------------------------------------
createCluster(
  area = "area01", 
  cluster_name = "clus01"
)
getVariantCommands(last = TRUE)

## ----create_cluster2----------------------------------------------------------
createCluster(
  area = "area01", 
  cluster_name = "clus02",
  unitcount = 1L,
  marginal_cost = 50,
  ts_interpretation = "production-factor",
  group = "Nuclear",
  add_prefix = FALSE,
  prepro_data = matrix(
    data = c(rep(9, times = 365 * 2),
             rep(7, times = 365 * 4)), 
    ncol = 6
  ),
  prepro_modulation = matrix(
    data = c(rep(8, times = 365 * 24 * 3),
             rep(6, times = 365 * 24 * 1)),
    ncol = 4
  ),
  time_series = matrix(
    data = c(rep(22, times = 365 * 24 * 8),
             rep(44, times = 365 * 24 * 4)),
    ncol = 12
  )
)
getVariantCommands(last = 2)

## ----edit_cluster, eval = FALSE-----------------------------------------------
#  createCluster(
#    area = "area02",
#    cluster_name = "clus02"
#  )
#  editCluster(
#    area = "area02",
#    cluster_name = "clus02",
#    unitcount = 5L
#  )
#  getVariantCommands(last = TRUE)

## ----remove_cluster-----------------------------------------------------------
createCluster(
  area = "area02", 
  cluster_name = "clus000"
)
removeCluster(
  area = "area02", 
  cluster_name = "clus000"
)
getVariantCommands(last = TRUE)

## ----create_binding_constraint------------------------------------------------
createBindingConstraint(
  name = "myconstraint",
  values = NULL,
  enabled = FALSE,
  timeStep = "hourly",
  operator = "both",
  coefficients = c("area01%area02" = 1)
)
getVariantCommands(last = TRUE)

## ----update_general_settings--------------------------------------------------
updateGeneralSettings(mode = "Adequacy", generate = c("thermal", "hydro"))
getVariantCommands(last = 2)

## ----update_input_settings----------------------------------------------------
updateInputSettings(import = c("hydro", "thermal"))
getVariantCommands(last = TRUE)

## ----update_optimization_settings---------------------------------------------
updateOptimizationSettings(
  simplex.range = "week",
  power.fluctuations = "minimize ramping"
)
getVariantCommands(last = 2)

## ----update_output_settings---------------------------------------------------
updateOutputSettings(
  synthesis = TRUE,
  storenewset = FALSE,
  archives = c("load", "wind")
)
getVariantCommands(last = TRUE)

## ----read_scenario_builder----------------------------------------------------
readScenarioBuilder()

## ----scenario_builder---------------------------------------------------------
my_scenario <- scenarioBuilder(n_scenario = 3, areas = c("area01", "area02"), n_mc = 10)
my_scenario

## ----update_scenario_builder--------------------------------------------------
updateScenarioBuilder(ldata = my_scenario, series = "load")
getVariantCommands(last = TRUE)

## ----write_input_ts-----------------------------------------------------------
writeInputTS("area01", type = "solar", data = matrix(rep(4, 8760*2), nrow = 8760))
getVariantCommands(last = TRUE)

## ----write_water_values-------------------------------------------------------
writeWaterValues("area01", data = matrix(rep(0, 365*101), nrow = 365))
getVariantCommands(last = TRUE)

