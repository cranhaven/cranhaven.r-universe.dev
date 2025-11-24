# Test integration (taken from vignette)

test_that("Worklow works as expected", {
  skip_on_os(os = "mac")
  skip_on_cran()

  bounds <- spm_as_boundary(boundaries = sfa_boundaries,
                            boundary = "sfa")

  biomass_dataset <-
    spm_as_dataset(borealis_simulated, name = "borealis",
                   density = "weight_per_km2",
                   time = "year_f",
                   coords = c('lon_dec','lat_dec'),
                   uniqueID = "uniqueID")

  predator_dataset <-
    spm_as_dataset(predator_simulated, name = "all_predators",
                   density = "weight_per_km2",
                   time = "year_f",
                   coords = c("lon_dec", "lat_dec"),
                   uniqueID = "uniqueID")

  bounds_voronoi <- bounds %>%
    spm_discretize(method = "tesselate_voronoi",
                   with = biomass_dataset,
                   nb_samples = 30)

  bounds_delaunay <- bounds %>%
    spm_discretize(method = "triangulate_delaunay", a = 1, q = 30)

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

  predator_smooth <- predator_dataset %>%
    spm_smooth(weight_per_km2 ~ smooth_time() + smooth_space(),
               boundaries = bounds_voronoi,
               drop.unused.levels = F, family=tw, method= "fREML")

  catch_dataset <-
    spm_as_dataset(catch_simulated, name = "catch_data",
                   biomass = "catch",
                   time = "year_f",
                   uniqueID = "uniqueID",
                   coords = c("lon_dec", "lat_dec"))

  biomass_smooth_w_catch <-
    spm_aggregate_catch(biomass = biomass_smooth,
                        catch = catch_dataset,
                        biomass_variable = "weight_per_km2",
                        catch_variable = "catch",
                        fill = mean)

  sspm_model <- sspm(biomass = biomass_smooth_w_catch,
                     predictors = predator_smooth)

  sspm_model <- sspm_model %>%
    spm_split(year_f %in% c(1990:2017))

  sspm_model <- sspm_model %>%
    spm_lag(vars = c("weight_per_km2_borealis",
                     "weight_per_km2_all_predators"),
            n = 1)

  sspm_model_fit <- sspm_model %>%
    spm(log_productivity ~ sfa +
          weight_per_km2_all_predators_lag_1 +
          smooth_space(by = weight_per_km2_borealis_lag_1) +
          smooth_space() +
          smooth_lag("weight_per_km2_borealis"),
        family = mgcv::scat)

  summary(sspm_model_fit, biomass = "weight_per_km2_borealis")

  gam_fit <- spm_get_fit(sspm_model_fit)

  preds <- predict(sspm_model_fit)

  biomass_preds <- predict(sspm_model_fit, biomass = "weight_per_km2_borealis")

  biomass_one_step <- predict(sspm_model_fit, biomass = "weight_per_km2_borealis",
                              next_ts = TRUE)

  ## Plots

  plot(bounds)
  plot(bounds_voronoi)
  plot(bounds_delaunay)
  plot(biomass_smooth, var = "weight_per_km2", log = FALSE, interval = T)
  plot(biomass_smooth, var = "weight_per_km2", use_sf = TRUE)
  plot(sspm_model_fit, train_test = TRUE, scales = "free")
  plot(sspm_model_fit, log = T, scales = 'free')
  plot(sspm_model_fit, log = T, use_sf = TRUE)
  plot(sspm_model_fit, biomass = "weight_per_km2_borealis",  scales = "free")
  plot(sspm_model_fit, biomass = "weight_per_km2_borealis", use_sf = TRUE)
  plot(sspm_model_fit, biomass = "weight_per_km2_borealis",
       next_ts = TRUE, aggregate = TRUE, scales = "free",
       smoothed_biomass = TRUE, interval = T)

  ## Closes with an expectation
  expect_data_frame(biomass_one_step)

})
