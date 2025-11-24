
# Load packages -----------------------------------------------------------

library(testthat)
library(checkmate)
library(dplyr)
library(sf)
library(mgcv)
library(sspm)

# Load package data -------------------------------------------------------

data(borealis_simulated, package = "sspm")
data(predator_simulated, package = "sspm")
data(sfa_boundaries, package = "sspm")
data(catch_simulated, package = "sspm")

borealis_patches <- sspm:::borealis_patches
borealis_points <- sspm:::borealis_points

borealis_spatial <- sspm:::borealis_spatial %>%
  dplyr::mutate(year = as.numeric(as.character(year_f)))

predator_spatial <- sspm:::predator_spatial %>%
  dplyr::mutate(year = as.numeric(as.character(year_f)))

borealis_spatial_joined <- borealis_spatial %>%
  st_join(dplyr::select(borealis_patches, "patch_id")) %>%
  dplyr::group_by("sfa", "year", "patch_id") %>%
  dplyr::slice_head(n=1) %>%
  dplyr::ungroup() %>%
  dplyr::mutate("row_ID" = 1:dplyr::n())

predator_spatial_joined <- predator_spatial %>%
  st_join(dplyr::select(borealis_patches, "patch_id")) %>%
  dplyr::group_by("sfa", "year", "patch_id") %>%
  dplyr::slice_head(n=1) %>%
  dplyr::ungroup() %>%
  dplyr::mutate("row_ID" = 1:dplyr::n())

# Create objects ----------------------------------------------------------

# Method
discret_method <- new("discretization_method",
                      name = "voronoi_method",
                      method = tesselate_voronoi)

# Boundaries
boundary <- new("sspm_boundary",
                boundaries = sfa_boundaries,
                boundary = "sfa",
                boundary_area = "area")

boundary_discrete <- new("sspm_discrete_boundary",
                         boundaries = sfa_boundaries,
                         boundary = "sfa",
                         method = discret_method,
                         patches = borealis_patches,
                         points = borealis_points,
                         boundary_area = "area",
                         patches_area = "patch_area")

# Base objects
biomass_dataset <- new("sspm_dataset",
                       data = borealis_spatial,
                       time = "year",
                       density = "weight_per_km2",
                       uniqueID = "uniqueID",
                       coords = c('lon_dec', 'lat_dec'))

predator_dataset <- new("sspm_dataset",
                        data = predator_spatial,
                        density = "weight_per_km2",
                        time = "year",
                        uniqueID = "uniqueID",
                        coords = c('lon_dec', 'lat_dec'))

catch_dataset <- new("sspm_dataset",
                     data = catch_simulated,
                     biomass = "catch",
                     time = "year",
                     uniqueID = "uniqueID",
                     coords = c('lon_dec', 'lat_dec'))

# Formula
sspm_formula <- new("sspm_formula",
                    raw_formula = as.formula("weight_per_km2 ~ smooth_time() +
                                               smooth_space() + smooth_space_time()"),
                    translated_formula = as.formula("weight_per_km2 ~ s(year_f,
                      k = 24L, bs = 're', xt = list(penalty = pen_mat_time)) +
                      s(patch_id, k = 30, bs = 'mrf', xt = list(penalty = pen_mat_space)) +
                      ti(year_f, patch_id, k = c(24, 30), bs = c('re', 'mrf'),
                      xt = list(year_f = list(penalty = pen_mat_time),
                      patch_id = list(penalty = pen_mat_space)))"),
                    vars = list(pen_mat_time = matrix(),
                                pen_mat_space = matrix()),
                    response = "weight_per_km2")

# Smoothed objects
biomass_dataset_smoothed <- new("sspm_dataset",
                                name = "biomass_test",
                                data = borealis_spatial,
                                density = "weight_per_km2",
                                boundaries = boundary_discrete,
                                time = "year",
                                uniqueID = "uniqueID",
                                coords = c('lon_dec', 'lat_dec'),
                                formulas = list(sspm_formula),
                                smoothed_vars = "weight_per_km2",
                                smoothed_data = borealis_spatial_joined,
                                smoothed_fit = list(mgcv::gam(mtcars$mpg~mtcars$cyl,
                                                              family = gaussian)),
                                is_mapped = TRUE)

predator_dataset_smoothed <- new("sspm_dataset",
                                 name = "predator_test",
                                 data = predator_spatial,
                                 density = "weight_per_km2",
                                 boundaries = boundary_discrete,
                                 time = "year",
                                 uniqueID = "uniqueID",
                                 coords = c('lon_dec', 'lat_dec'),
                                 formulas = list(sspm_formula),
                                 smoothed_vars = "weight_per_km2",
                                 smoothed_data = predator_spatial_joined,
                                 smoothed_fit = list(mgcv::gam(mtcars$mpg~mtcars$cyl,
                                                               family = gaussian)),
                                 is_mapped = TRUE)

# -------------------------------------------------------------------------

all_data <- list(biomass = biomass_dataset_smoothed,
                 predator = predator_dataset_smoothed,
                 catch = catch_dataset)

sspm_model <- new("sspm",
                  datasets = all_data,
                  time = spm_time(biomass_dataset_smoothed),
                  uniqueID = "row_ID",
                  boundaries = spm_boundaries(biomass_dataset_smoothed),
                  smoothed_data = borealis_spatial,
                  is_split = FALSE)

fit_bam <- bam(data = mtcars, mpg ~ wt, family = gaussian)

sspm_fit <- new("sspm_fit",
                smoothed_data = all_data,
                time = spm_time(biomass_dataset_smoothed),
                uniqueID = spm_unique_ID(biomass_dataset_smoothed),
                formula = sspm_formula,
                boundaries = spm_boundaries(biomass_dataset_smoothed),
                fit = fit_bam)
