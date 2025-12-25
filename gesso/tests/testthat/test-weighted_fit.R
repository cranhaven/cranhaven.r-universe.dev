context("weighted fit")

test_that("fit with half zero weights", {
  grid_size = 10
  max_iterations = 10000
  sample_size = 100
  n_g_non_zero = 10
  n_gxe_non_zero = 5
  for (seed in 1:2) {
    for (family in c("gaussian", "binomial")) {
      if (family == "gaussian") {
        p = 30
        tol = 1e-3
      } else {
        p = 100
        tol = 1e-3
      }
      data = data.gen(seed=seed, family=family,
                      p=p, sample_size=sample_size,
                      n_g_non_zero=n_g_non_zero,
                      n_gxe_non_zero=n_gxe_non_zero,
                      normalize=FALSE)
      half_n = sample_size / 2
      half_samples = sort(sample(sample_size, half_n, replace=FALSE))
      weights = rep(0, sample_size)
      weights[half_samples] = 1
      weights = weights / half_n
      
      fit_full_data = gesso.fit(data$G_train,
                                     data$E_train,
                                     data$Y_train,
                                     tolerance=tol,
                                     grid_size=grid_size,
                                     family=family,
                                     max_iterations=max_iterations,
                                     weights=weights,
                                     normalize=TRUE)
      expect_equal(sum(fit_full_data$has_converged != 1), 0)
      
      fit_half_data = gesso.fit(data$G_train[half_samples,],
                                     data$E_train[half_samples],
                                     data$Y_train[half_samples],
                                     tolerance=tol,
                                     grid=fit_full_data$grid,
                                     family=family,
                                     max_iterations=max_iterations,
                                     normalize=TRUE)
      expect_equal(sum(fit_half_data$has_converged != 1), 0)
      
      
      expect_lt(max(abs(fit_full_data$objective_value - fit_half_data$objective_value)), 1e-10)
      expect_lt(max(abs(fit_full_data$beta_0 - fit_half_data$beta_0)), 1e-10)
      expect_lt(max(abs(fit_full_data$beta_e - fit_half_data$beta_e)), 1e-10)
      expect_lt(max(abs(fit_full_data$beta_g - fit_half_data$beta_g)), 1e-10)
      expect_lt(max(abs(fit_full_data$beta_gxe - fit_half_data$beta_gxe)), 1e-10)
    }
  }
})

