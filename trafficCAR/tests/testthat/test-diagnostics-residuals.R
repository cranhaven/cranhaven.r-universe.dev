test_that("residuals compute correctly", {
  fit <- list(
    y = c(1, 2, 3),
    mu = c(1.1, 1.9, 3.2),
    x = c(0.2, -0.1, 0.0)
  )
  class(fit) <- "traffic_fit"

  r_raw <- residuals(fit, "raw")
  expect_equal(as.numeric(r_raw), c(-0.1, 0.1, -0.2))

  r_s <- residuals(fit, "structured")
  expect_equal(as.numeric(r_s), fit$x)

  r_u <- residuals(fit, "unstructured")
  expect_equal(
    as.numeric(r_u),
    fit$y - (fit$mu - fit$x)
  )
})



test_that("residuals carry correct metadata", {
  fit <- list(
    y = c(1, 2, 3),
    mu = c(1.1, 1.9, 3.2),
    x = c(0.2, -0.1, 0.0)
  )
  class(fit) <- "traffic_fit"

  r <- residuals(fit, "raw")

  expect_s3_class(r, "traffic_residuals")
  expect_equal(attr(r, "type"), "raw")
  expect_equal(attr(r, "n"), 3L)
})
