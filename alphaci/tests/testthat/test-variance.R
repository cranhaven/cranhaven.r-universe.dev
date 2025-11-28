k <- 5
rho <- 0.5
phi <- 3
sigma <- matrix(phi^2 * rho, k, k)
diag(sigma) <- phi^2
x <- simulate_congeneric(10, k = k, lambda = runif(5), sigma = runif(3))

test_that("avar_parallel consistet with others", {
  gamma <- gamma_mat(x, sigma, type = "normal")
  expect_equal(
    avar_parallel_elliptical(alpha(sigma), x, type = "normal"),
    avar(x, sigma, type = "normal", parallel = TRUE)
  )
})

test_that("same with `parallel = TRUE` and `FALSE` under normal parallel", {
  expect_equal(
    avar_elliptical(x, sigma, type = "normal"),
    avar_parallel_elliptical(alpha(sigma), x, type = "normal")
  )
})

test_that("avar equivalent to var_ell under parallell normality", {
  expect_equal(
    avar(x, sigma, type = "normal", parallel = TRUE),
    avar_parallel_elliptical(alpha(sigma), x, type = "normal")
  )
})

test_that("avar yields different results.", {
  results <- c(
    avar(x, cov(x), type = "adf", parallel = FALSE),
    avar(x, cov(x), type = "adf", parallel = TRUE),
    avar(x, cov(x), type = "elliptical", parallel = FALSE),
    avar(x, cov(x), type = "elliptical", parallel = TRUE),
    avar(x, cov(x), type = "normal", parallel = FALSE),
    avar(x, cov(x), type = "normal", parallel = TRUE)
  )
  for (i in seq(length(results) - 1)) {
    expect_false(isTRUE(all.equal(results[i], results[i + 1])))
  }
})

test_that("avar_std yields different results.", {
  results <- c(
    avar_std(x, cov(x), type = "adf", parallel = FALSE),
    avar_std(x, cov(x), type = "adf", parallel = TRUE),
    avar_std(x, cov(x), type = "elliptical", parallel = FALSE),
    avar_std(x, cov(x), type = "elliptical", parallel = TRUE),
    avar_std(x, cov(x), type = "normal", parallel = FALSE),
    avar_std(x, cov(x), type = "normal", parallel = TRUE)
  )
  for (i in seq(length(results) - 1)) {
    expect_false(isTRUE(all.equal(results[i], results[i + 1])))
  }
})
