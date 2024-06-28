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

## -----------------------------------------------------------------------------
path <- tempdir()
createStudy(path = path, study_name = "my-study")

# Set number of Monte-Carlo scenarios
updateGeneralSettings(nbyears = 10)

# First area
createArea("earth")
createCluster(area = "earth", cluster_name = "america", add_prefix = FALSE)
createCluster(area = "earth", cluster_name = "africa", add_prefix = FALSE)
createCluster(area = "earth", cluster_name = "europe", add_prefix = FALSE)

# Second one
createArea("moon")
createCluster(area = "moon", cluster_name = "tranquility", add_prefix = FALSE)
createCluster(area = "moon", cluster_name = "serenety", add_prefix = FALSE)

# More areas
createArea("titan")
createArea("ceres")

# Some links
createLink("earth", "moon")
createLink("moon", "titan")
createLink("moon", "ceres")

# Check what we have created
getAreas()
readClusterDesc()

## -----------------------------------------------------------------------------
readScenarioBuilder()

## -----------------------------------------------------------------------------
# All areas
scenarioBuilder(n_scenario = 3)
scenarioBuilder(n_scenario = 5)
# Specific area
scenarioBuilder(n_scenario = 3, areas = "earth")
# Specify an area for which to use random values
scenarioBuilder(n_scenario = 3, areas_rand = "earth")

## -----------------------------------------------------------------------------
my_scenario <- scenarioBuilder(n_scenario = 3)

# for load serie
updateScenarioBuilder(ldata = my_scenario, series = "load")

# equivalent as
updateScenarioBuilder(ldata = list(l = my_scenario))

## -----------------------------------------------------------------------------
my_scenario <- scenarioBuilder(n_scenario = 3)

updateScenarioBuilder(
  ldata = my_scenario, 
  series = c("load", "hydro", "solar")
)

## -----------------------------------------------------------------------------
load_scenario <- scenarioBuilder(n_scenario = 3)
hydro_scenario <- scenarioBuilder(n_scenario = 4)
solar_scenario <- scenarioBuilder(n_scenario = 5)

updateScenarioBuilder(ldata = list(
  l = load_scenario,
  h = hydro_scenario,
  s = solar_scenario
))

## -----------------------------------------------------------------------------
readScenarioBuilder()

## -----------------------------------------------------------------------------
my_scenario <- scenarioBuilder(n_scenario = 3)

updateScenarioBuilder(
  ldata = my_scenario, 
  series = "thermal"
)

readScenarioBuilder()$t

## -----------------------------------------------------------------------------
updateScenarioBuilder(
  ldata = my_scenario, 
  series = "thermal",
  clusters_areas = data.table::data.table(
    area = c("earth", "earth"),
    cluster = c("africa", "europe")
  )
)
readScenarioBuilder()$t

## -----------------------------------------------------------------------------
updateScenarioBuilder(
  ldata = my_scenario, 
  series = "ntc"
)
readScenarioBuilder()$ntc

## -----------------------------------------------------------------------------
updateScenarioBuilder(
  ldata = my_scenario, 
  series = "ntc",
  links = "moon%ceres"
)
readScenarioBuilder()$ntc

## -----------------------------------------------------------------------------
clearScenarioBuilder()

## ----echo=FALSE---------------------------------------------------------------
unlink(file.path(path, "my-study"), recursive = TRUE)

