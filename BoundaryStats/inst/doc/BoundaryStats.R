## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----message = FALSE----------------------------------------------------------
library(BoundaryStats)
library(terra)
library(magrittr)

## ----fig.width = 8, fig.height = 6--------------------------------------------
data(ecoregions)
ecoregions <- rast(ecoregions_matrix, crs = ecoregions_crs)
ext(ecoregions) <- ecoregions_ext
plot(ecoregions)

## ----fig.width = 8, fig.height = 6--------------------------------------------
data(L.flavomaculatus)
L.flavomaculatus <- rast(L.flavomaculatus_matrix, crs = L.flavomaculatus_crs)
ext(L.flavomaculatus) <- L.flavomaculatus_ext
plot(L.flavomaculatus)

## -----------------------------------------------------------------------------
crs(ecoregions) <- crs(L.flavomaculatus)
ecoregions <- resample(ecoregions, L.flavomaculatus) %>%
  crop(., L.flavomaculatus) %>%
  mask(., L.flavomaculatus)
L.flavomaculatus <- crop(L.flavomaculatus, ecoregions) %>%
  mask(., ecoregions)

## ----fig.width = 8, fig.height = 6--------------------------------------------
ecoregions_boundaries <- categorical_boundary(ecoregions)
L.flavomaculatus_boundaries <- categorical_boundary(L.flavomaculatus)

## ----warning = FALSE, fig.width = 8, fig.height = 6---------------------------
plot_boundary(L.flavomaculatus_boundaries, ecoregions_boundaries, trait_names = c('A. delicatus genetic group', 'Ecoregion'))

## -----------------------------------------------------------------------------
L.flav_bound.null <- boundary_null_distrib(L.flavomaculatus, cat = T, n_iterations = 10, model = 'random_cluster', p = 0.5, progress = F)

## -----------------------------------------------------------------------------
n_subgraph(L.flavomaculatus_boundaries, L.flav_bound.null)
max_subgraph(L.flavomaculatus_boundaries, L.flav_bound.null)

## -----------------------------------------------------------------------------
L.flav_overlap.null <- overlap_null_distrib(L.flavomaculatus, ecoregions, rand_both = F, x_cat = T, n_iterations = 10, x_model = 'random_cluster', px = 0.5, progress = F)

## -----------------------------------------------------------------------------
Odirect(L.flavomaculatus_boundaries, ecoregions_boundaries, L.flav_overlap.null)
Ox(L.flavomaculatus_boundaries, ecoregions_boundaries, L.flav_overlap.null)
Oxy(L.flavomaculatus_boundaries, ecoregions_boundaries, L.flav_overlap.null)

