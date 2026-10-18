test_that("iglm handles empty overlap matrices gracefully", {
  n_units <- 10
  neighborhood <- matrix(1, n_units, n_units)
  xyz_obj <- iglm.data(neighborhood = neighborhood, n_units = n_units, return_neighborhood = FALSE, directed = FALSE)
  
  # The sampler_z tries to sample from the overlap_mat. 
  # If empty, this could cause out-of-bounds access in C++ without guards.
  sampler_new <- sampler.iglm(
    n_burn_in = 2, n_simulation = 2,
    sampler_z = sampler.net.attr(n_proposals = 100, tnt = TRUE)
  )
  
  model <- iglm(
    formula = xyz_obj ~ edges(mode = "local"),
    coef = -2, 
    sampler = sampler_new,
    control = control.iglm(max_it = 2, display_progress = FALSE)
  )
  
  # Verify that simulation run without crashing or erroring
  expect_no_error(model$simulate())
})

test_that("iglm handles empty overlap matrices with degrees gracefully", {
  n_units <- 5
  neighborhood <- matrix(1, n_units, n_units)
  xyz_obj <- iglm.data(neighborhood = neighborhood, n_units = n_units, return_neighborhood = FALSE, directed = FALSE)
  
  # Test with degrees sampler
  sampler_new <- sampler.iglm(
    n_burn_in = 2, n_simulation = 2,
    sampler_z = sampler.net.attr(n_proposals = 100, tnt = TRUE)
  )
  
  model <- iglm(
    formula = xyz_obj ~ edges(mode = "local") + degrees,
    coef = -2,
    coef_degrees = rep(-1, n_units),
    sampler = sampler_new,
    control = control.iglm(max_it = 2, display_progress = FALSE)
  )
  expect_no_error(model$simulate())
})
