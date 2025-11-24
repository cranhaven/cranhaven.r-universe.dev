## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(sspm)
library(mgcv)
library(dplyr)

## ----data---------------------------------------------------------------------
sfa_boundaries
borealis_simulated
predator_simulated
catch_simulated

## ----boundaries, fig.width=7, fig.height=5, fig.fullwidth=TRUE, fig.align='center'----
bounds <- spm_as_boundary(boundaries = sfa_boundaries, 
                          boundary = "sfa")

plot(bounds)

## -----------------------------------------------------------------------------
biomass_dataset <- 
  spm_as_dataset(borealis_simulated, name = "borealis",
                 density = "weight_per_km2",
                 time = "year_f",
                 coords = c('lon_dec','lat_dec'), 
                 uniqueID = "uniqueID")

biomass_dataset

## -----------------------------------------------------------------------------
predator_dataset <- 
  spm_as_dataset(predator_simulated, name = "all_predators", 
                 density = "weight_per_km2",
                 time = "year_f",
                 coords = c("lon_dec", "lat_dec"),
                 uniqueID = "uniqueID")

predator_dataset

## -----------------------------------------------------------------------------
bounds_voronoi <- bounds %>% 
  spm_discretize(method = "tesselate_voronoi",
                 with = biomass_dataset, 
                 nb_samples = 30)

bounds_voronoi

## ----eval = FALSE-------------------------------------------------------------
# ## Not run
# bounds_delaunay <- bounds %>%
#   spm_discretize(method = "triangulate_delaunay", a = 1, q = 30)
# bounds_delaunay

## ----fig.width=7, fig.height=5, fig.fullwidth=TRUE, fig.align='center'--------
plot(bounds_voronoi)

## ----eval = FALSE-------------------------------------------------------------
# ## Not run
# plot(bounds_delaunay)

## -----------------------------------------------------------------------------
spm_patches(bounds_voronoi)
spm_points(bounds_voronoi)

## -----------------------------------------------------------------------------
biomass_smooth <- biomass_dataset %>%  
  spm_smooth(weight_per_km2 ~ sfa + smooth_time(by = sfa) + 
               smooth_space() + 
               smooth_space_time(),
             boundaries = bounds_voronoi, 
             family=tw)%>% 
  spm_smooth(temp_at_bottom ~ smooth_time(by=sfa, xt = NULL) +
               smooth_space() +
               smooth_space_time(xt = NULL),
             family=gaussian)

biomass_smooth

## ----fig.width=7, fig.height=5, fig.fullwidth=TRUE, fig.align='center'--------
plot(biomass_smooth, var = "weight_per_km2", log = FALSE, interval = T)

## ----fig.width=7, fig.height=5, fig.fullwidth=TRUE, fig.align='center'--------
plot(biomass_smooth, var = "weight_per_km2", use_sf = TRUE)

## -----------------------------------------------------------------------------
predator_smooth <- predator_dataset %>%  
  spm_smooth(weight_per_km2 ~ smooth_time() + smooth_space(),
             boundaries = bounds_voronoi,
             drop.unused.levels = F, family=tw, method= "fREML")

predator_smooth

## -----------------------------------------------------------------------------
catch_dataset <- 
  spm_as_dataset(catch_simulated, name = "catch_data", 
                 biomass = "catch",
                 time = "year_f", 
                 uniqueID = "uniqueID", 
                 coords = c("lon_dec", "lat_dec"))

catch_dataset

## -----------------------------------------------------------------------------
biomass_smooth_w_catch <- 
  spm_aggregate_catch(biomass = biomass_smooth, 
                      catch = catch_dataset, 
                      biomass_variable = "weight_per_km2",
                      catch_variable = "catch",
                      fill = mean)

biomass_smooth_w_catch

## -----------------------------------------------------------------------------
sspm_model <- sspm(biomass = biomass_smooth_w_catch, 
                   predictors = predator_smooth)

sspm_model

## -----------------------------------------------------------------------------
sspm_model <- sspm_model %>% 
  spm_split(year_f %in% c(1990:2017))

sspm_model

## -----------------------------------------------------------------------------
sspm_model <- sspm_model %>% 
  spm_lag(vars = c("weight_per_km2_borealis", 
                   "weight_per_km2_all_predators"), 
          n = 1)

sspm_model

## -----------------------------------------------------------------------------
sspm_model_fit <- sspm_model %>% 
  spm(log_productivity ~ sfa +
        weight_per_km2_all_predators_lag_1 +
        smooth_space(by = weight_per_km2_borealis_lag_1) +
        smooth_space(), 
      family = mgcv::scat)

sspm_model_fit

## -----------------------------------------------------------------------------
gam_fit <- spm_get_fit(sspm_model_fit)
summary(gam_fit)

## -----------------------------------------------------------------------------
summary(sspm_model_fit, biomass = "weight_per_km2_borealis")

## ----fig.width=7, fig.height=5, fig.fullwidth=TRUE, fig.align='center'--------
plot(sspm_model_fit, train_test = TRUE, scales = "free")

## -----------------------------------------------------------------------------
preds <- predict(sspm_model_fit)
head(preds)

## -----------------------------------------------------------------------------
biomass_preds <- predict(sspm_model_fit, biomass = "weight_per_km2_borealis")
head(biomass_preds)

## -----------------------------------------------------------------------------
biomass_one_step <- predict(sspm_model_fit, biomass = "weight_per_km2_borealis", 
                            next_ts = TRUE)
head(biomass_one_step)

## ----fig.width=7, fig.height=5, fig.fullwidth=TRUE, fig.align='center'--------
plot(sspm_model_fit, log = T, scales = 'free')
plot(sspm_model_fit, log = T, use_sf = TRUE)

## ----fig.width=7, fig.height=5, fig.fullwidth=TRUE, fig.align='center'--------
plot(sspm_model_fit, biomass = "weight_per_km2_borealis",  scales = "free")
plot(sspm_model_fit, biomass = "weight_per_km2_borealis", use_sf = TRUE)

## ----fig.width=7, fig.height=5, fig.fullwidth=TRUE, fig.align='center'--------
plot(sspm_model_fit, biomass = "weight_per_km2_borealis",
     next_ts = TRUE, aggregate = TRUE, scales = "free", 
     smoothed_biomass = TRUE, interval = T)

