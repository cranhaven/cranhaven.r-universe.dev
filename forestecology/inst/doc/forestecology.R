## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.path = "man/figures/README-",
  out.width = "100%",
  fig.width = 16 / 2,
  fig.height = 9 / 2,
  message = FALSE,
  warning = FALSE,
  cache = FALSE
)
set.seed(76)

## -----------------------------------------------------------------------------
library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)
library(forestecology)
library(patchwork)
library(blockCV)

# Resolve conflicting functions
filter <- dplyr::filter

## -----------------------------------------------------------------------------
census_1_ex

## -----------------------------------------------------------------------------
ggplot() +
  geom_sf(
    data = census_1_ex %>% sf::st_as_sf(coords = c("gx", "gy")),
    aes(col = sp, size = dbh)
  )

## -----------------------------------------------------------------------------
growth_ex <-
  compute_growth(
    census_1 = census_1_ex %>% 
      mutate(sp = to_any_case(sp) %>% factor()),
    census_2 = census_2_ex %>% 
      filter(!str_detect(codes, "R")) %>% 
      mutate(sp = to_any_case(sp) %>% factor()),
    id = "ID"
  ) %>% 
  # Compute basal area:
  mutate(basal_area = 0.0001 * pi * (dbh1 / 2)^2)

## -----------------------------------------------------------------------------
# Set competitor distance
comp_dist <- 1

# Add buffer variable to growth data frame
growth_ex <- growth_ex %>%
  add_buffer_variable(size = comp_dist, region = study_region_ex)

# Optional: Create sf representation of buffer region
buffer_region <- study_region_ex %>% 
  compute_buffer_region(size = comp_dist)

## -----------------------------------------------------------------------------
base_plot <- ggplot() +
  geom_sf(data = study_region_ex, fill = "transparent") +
  geom_sf(data = buffer_region, fill = "transparent", linetype = "dashed")

base_plot + 
  geom_sf(data = growth_ex, aes(col = buffer), size = 2)

## -----------------------------------------------------------------------------
fold1 <- rbind(c(0, 0), c(5, 0), c(5, 5), c(0, 5), c(0, 0))
fold2 <- rbind(c(5, 0), c(10, 0), c(10, 5), c(5, 5), c(5, 0))

blocks_ex <- bind_rows(
  sf_polygon(fold1),
  sf_polygon(fold2)
) %>%
  mutate(folds = c(1, 2) %>% factor())

## -----------------------------------------------------------------------------
SpatialBlock_ex <- blockCV::spatialBlock(
  speciesData = growth_ex, k = 2, selection = "systematic", blocks = blocks_ex,
  showBlocks = FALSE, verbose = FALSE
)

growth_ex <- growth_ex %>%
  mutate(foldID = SpatialBlock_ex$foldID %>% factor())

## -----------------------------------------------------------------------------
base_plot + 
  geom_sf(data = growth_ex, aes(col = buffer, shape = foldID), size = 2) +
  geom_sf(data = blocks_ex, fill = "transparent", col = "orange")

## -----------------------------------------------------------------------------
focal_vs_comp_ex <- growth_ex %>%
  create_focal_vs_comp(comp_dist, blocks = blocks_ex, id = "ID", comp_x_var = "basal_area")
focal_vs_comp_ex

## -----------------------------------------------------------------------------
focal_vs_comp_ex %>% 
  unnest(cols = "comp")

## -----------------------------------------------------------------------------
comp_bayes_lm_ex <- focal_vs_comp_ex %>%
  comp_bayes_lm(prior_param = NULL)

## -----------------------------------------------------------------------------
# Print
comp_bayes_lm_ex

# Posterior distributions (plots combined with patchwork pkg)
p1 <- autoplot(comp_bayes_lm_ex, type = "intercepts")
p2 <- autoplot(comp_bayes_lm_ex, type = "dbh_slopes")
p3 <- autoplot(comp_bayes_lm_ex, type = "competition")
(p1 | p2) / p3

## -----------------------------------------------------------------------------
focal_vs_comp_ex <- focal_vs_comp_ex %>%
  mutate(growth_hat = predict(comp_bayes_lm_ex, newdata = focal_vs_comp_ex))
focal_vs_comp_ex

## -----------------------------------------------------------------------------
focal_vs_comp_ex %>%
  rmse(truth = growth, estimate = growth_hat) %>%
  pull(.estimate)

## -----------------------------------------------------------------------------
focal_vs_comp_ex <- focal_vs_comp_ex %>%
  run_cv(comp_dist = comp_dist, blocks = blocks_ex)

## -----------------------------------------------------------------------------
focal_vs_comp_ex %>%
  rmse(truth = growth, estimate = growth_hat) %>%
  pull(.estimate)

