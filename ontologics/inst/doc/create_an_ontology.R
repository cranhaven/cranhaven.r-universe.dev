## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
library(knitr)

## ----setup--------------------------------------------------------------------
library(ontologics)
library(dplyr, warn.conflicts = FALSE)

## -----------------------------------------------------------------------------
# read in example ontology
crops <- load_ontology(path = system.file("extdata", "crops.rds", package = "ontologics"))

crops   # ... has a pretty show-method

## -----------------------------------------------------------------------------
lulc <- start_ontology(name = "land_surface_properties",
                       version = "0.0.1",
                       path = tempdir(), 
                       code = ".xx", 
                       description = "showcase of the ontologics R-package", 
                       homepage = "https://github.com/luckinet/ontologics", 
                       license = "CC-BY-4.0")

lulc # nothing included so far

## -----------------------------------------------------------------------------
kable(lulc@sources)

## -----------------------------------------------------------------------------
# currently it is only possible to set one class at a time
lulc <- new_class(
  new = "landcover", 
  target = NA, 
  description = "A good definition of landcover",
  ontology = lulc)

lulc <- new_class(
    new = "land use", 
    target = "landcover", 
    description = "A good definition of land use",
    ontology = lulc)

# the class IDs are derived from the code that was previously specified 
kable(lulc@classes$harmonised[, 1:6])

## -----------------------------------------------------------------------------
lc <- c(
  "Urban fabric", "Industrial, commercial and transport units",
  "Mine, dump and construction sites", "Artificial, non-agricultural vegetated areas",
  "Temporary cropland", "Permanent cropland", "Heterogeneous agricultural areas",
  "Forests", "Other Wooded Areas", "Shrubland", "Herbaceous associations",
  "Heterogeneous semi-natural areas", "Open spaces with little or no vegetation",
  "Inland wetlands", "Marine wetlands", "Inland waters", "Marine waters"
)

lulc <- new_concept(
  new = lc,
  class = "landcover",
  ontology = lulc
)

kable(lulc@concepts$harmonised[, 1:5])

## -----------------------------------------------------------------------------
lu <- tibble(
  concept = c(
    "Fallow", "Herbaceous crops", "Temporary grazing",
    "Permanent grazing", "Shrub orchards", "Palm plantations",
    "Tree orchards", "Woody plantation", "Protective cover",
    "Agroforestry", "Mosaic of agricultural-uses",
    "Mosaic of agriculture and natural vegetation",
    "Undisturbed Forest", "Naturally Regenerating Forest",
    "Planted Forest", "Temporally Unstocked Forest"
  ),
  broader = c(
    rep(lc[5], 3), rep(lc[6], 6),
    rep(lc[7], 3), rep(lc[8], 4)
  )
)



lulc <- get_concept(label = lu$broader, ontology = lulc) %>% 
  left_join(lu %>% select(label = broader), .) %>% 
  new_concept(
    new = lu$concept,
    broader = .,
    class = "land use",
    ontology = lulc
  )

kable(lulc@concepts$harmonised[, 1:5])

