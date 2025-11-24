# Test fit function

test_that("Fitting works as expected", {

  fit <- biomass_dataset %>%
    spm_smooth(weight_per_km2 ~ sfa + smooth_time(by=sfa) + smooth_space() +
                 smooth_space_time(k = c(NA, 30)),
               boundaries = boundary_discrete,
               family=tw) %>%
    spm_smooth(temp_at_bottom ~ smooth_time(by=sfa) + smooth_space() +
                 smooth_space_time(k = c(NA, 30)),
               family=gaussian)
  intercept <-
    fit@smoothed_fit$weight_per_km2$coefficients[[1]]
  expect_equal(intercept, 7.855774, tolerance = 1e-5)
})
