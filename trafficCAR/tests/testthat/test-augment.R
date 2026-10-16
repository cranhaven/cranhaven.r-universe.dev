
.make_fake_traffic_fit <- function(n = 4, p = 1, S = 20) {
  x <- matrix(rnorm(S * n), S, n)
  beta <- matrix(rnorm(S * p), S, p)
  sigma2 <- rgamma(S, 2, 2)

  base_fit <- structure(
    list(
      draws = list(x = x, beta = beta, sigma2 = sigma2),
      keep = seq_len(S),
      type = "proper",
      rho = 0.9,
      tau = 1
    ),
    class = "trafficCAR_fit"
  )

  structure(
    list(
      fit = base_fit,
      X = matrix(1, n, p),
      segment_id = seq_len(n),
      segment_id_col = "segment_id",
      transform_meta = list(
        inv = function(mu) mu,
        inv_interval = function(lo, hi) c(lo, hi)
      )
    ),
    class = "traffic_fit"
  )
}


test_that("extract_gaussian_draws accepts fit_car structure", {
  tf <- .make_fake_traffic_fit(n = 3, p = 2, S = 10)
  d <- .extract_gaussian_draws(tf$fit)

  expect_true(is.matrix(d$x))
  expect_true(is.matrix(d$beta))
  expect_true(is.numeric(d$sigma2))
  expect_equal(nrow(d$x), length(d$sigma2))
})

test_that("extract_gaussian_draws rejects malformed inputs", {
  expect_error(.extract_gaussian_draws(list(draws = NULL)), "draws")

  expect_error(
    .extract_gaussian_draws(list(draws = list(x = 1, beta = NULL, sigma2 = 1:3))),
    "matrix"
  )

  expect_error(
    .extract_gaussian_draws(list(draws = list(
      x = matrix(0, 2, 3), beta = NULL, sigma2 = 1:3
    ))),
    "nrow"
  )
})



test_that("augment_roads joins and adds traffic-interpretable columns", {
  tf <- .make_fake_traffic_fit(n = 5, p = 1, S = 30)
  roads <- data.frame(segment_id = 1:5)

  out <- augment_roads(tf, roads)

  expect_true(all(c(
    "predicted_mean",
    "predicted_lo",
    "predicted_hi",
    "relative_congestion"
  ) %in% names(out)))

  expect_equal(nrow(out), nrow(roads))
  expect_true(all(is.finite(out$predicted_mean)))
  expect_true(all(is.finite(out$relative_congestion)))
})



test_that("augment_roads rejects adversarial inputs", {
  tf <- .make_fake_traffic_fit(n = 3, p = 1, S = 10)

  expect_error(
    augment_roads(tf, data.frame(other = 1:3)),
    "join column"
  )

  tf_bad <- tf
  tf_bad$X <- NULL
  expect_error(augment_roads(tf_bad, data.frame(segment_id = 1:3)), "`fit\\$X`")
})


test_that("augment_roads handles NULL beta and extreme spatial effects", {
  tf <- .make_fake_traffic_fit(n = 4, p = 1, S = 20)
  tf$fit$draws$beta <- NULL
  tf$fit$draws$x[1, ] <- c(-1e6, 0, 1e6, 1e-9)

  out <- augment_roads(tf, data.frame(segment_id = 1:4))

  expect_true(all(is.finite(out$predicted_mean)))
  expect_true(all(is.finite(out$relative_congestion)))
})



test_that("augment_roads log back-transform never produces negative predictions", {
  tf <- .make_fake_traffic_fit(n = 4, p = 1, S = 25)

  tf$transform_meta <- list(
    inv = function(mu) pmax(exp(mu) - 1e-6, 0),
    inv_interval = function(lo, hi) c(
      pmax(exp(lo) - 1e-6, 0),
      pmax(exp(hi) - 1e-6, 0)
    )
  )

  tf$fit$draws$x <- matrix(
    rnorm(25 * 4, mean = -20, sd = 0.2),
    25, 4
  )

  out <- augment_roads(tf, data.frame(segment_id = 1:4))

  expect_true(all(out$predicted_mean >= 0))
  expect_true(all(out$predicted_lo >= 0))
  expect_true(all(out$predicted_hi >= 0))
})


