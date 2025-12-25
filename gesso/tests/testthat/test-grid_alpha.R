context("1D grid with alpha")

test_that("1D grid with lambda_2 = alpha * lambda_1 works correctly", {
  tol = 1e-4
  grid_size = 10
  max_iterations = 4000

  for (seed in 1:2) {
    for (family in c("gaussian", "binomial")) {
      for (alpha in c(0, 0.1, 0.5, 2)) {
        data = data.gen(seed=seed, family=family)
        
        fit = gesso.fit(G=data$G_train,
                        E=data$E_train,
                        Y=data$Y_train,
                        tolerance=tol,
                        grid_size=grid_size,
                        alpha=alpha,
                        family=family,
                        max_iterations=max_iterations,
                        normalize=TRUE)
        
        expect_lt(max(abs(fit$lambda_1 * alpha - fit$lambda_2)), 1e-12)
      }
    }
  }
})
