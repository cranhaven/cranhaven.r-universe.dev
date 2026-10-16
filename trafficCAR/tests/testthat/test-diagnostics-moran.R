
test_that("Moran's I runs and returns valid output", {
  A <- Matrix::sparseMatrix(
    i = c(1, 2, 2, 3),
    j = c(2, 1, 3, 2),
    x = 1,
    dims = c(3, 3)
  )

  fit <- list(
    y = c(1, 2, 3),
    mu = c(1, 2, 3),
    x = c(0.5, -0.2, -0.3),
    A = A
  )
  class(fit) <- "traffic_fit"

  m <- moran_residuals(fit, type = "structured", method = "analytic")
  expect_s3_class(m, "traffic_moran")
  expect_true(is.numeric(m$I))
})


test_that("Permutation Moran returns p-value", {
  A <- Matrix::sparseMatrix(
    i = c(1, 2, 2, 3),
    j = c(2, 1, 3, 2),
    x = 1,
    dims = c(3, 3)
  )

  fit <- list(
    y = c(1, 2, 3),
    mu = c(1, 2, 3),
    x = c(0.5, -0.2, -0.3),
    A = A
  )
  class(fit) <- "traffic_fit"

  m <- moran_residuals(fit, nsim = 49)
  expect_true(is.na(m$p_value) || (m$p_value >= 0 && m$p_value <= 1))

})
