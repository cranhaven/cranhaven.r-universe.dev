## ----setup, include=FALSE-----------------------------------------------------
set.seed(42)
library("mlr3")
library("mlr3spatiotempcv")

knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----include=FALSE------------------------------------------------------------
library(mlr3spatiotempcv)

## ----08-special-spatiotemp-002------------------------------------------------
# create 'sf' object
data_sf = sf::st_as_sf(ecuador, coords = c("x", "y"), crs = 32717)

# create `TaskClassifST` from `sf` object
task = as_task_classif_st(data_sf, id = "ecuador_task", target = "slides", positive = "TRUE")

## ----08-special-spatiotemp-003------------------------------------------------
task = as_task_classif_st(ecuador, id = "ecuador_task", target = "slides",
  positive = "TRUE", coordinate_names = c("x", "y"), crs = 32717)

## ----08-special-spatiotemp-004------------------------------------------------
print(task)

## -----------------------------------------------------------------------------
mlr_reflections$task_types

## -----------------------------------------------------------------------------
mlr_reflections$task_col_roles

