test_that("prep_speed log transform + metadata works", {
  x <- c(0, 5, 10)
  out <- prep_speed(x, transform = "log", eps = 1e-6)

  expect_true(is.list(out))
  expect_true(is.numeric(out$y))
  expect_equal(length(out$y), length(x))
  expect_equal(out$meta$outcome, "speed")
  expect_equal(out$meta$transform, "log")

  mu <- out$y
  bt <- out$meta$inv(mu)
  expect_true(all(bt >= 0))
  expect_equal(length(bt), length(x))
})



test_that("prep_speed handles adversarial inputs", {
  expect_error(prep_speed("a"), "numeric")
  expect_error(prep_speed(c(1, NA)), "finite")
  expect_error(prep_speed(c(1, Inf)), "finite")
  expect_error(prep_speed(c(-1, 2)), "nonnegative")
  expect_error(prep_speed(c(1, 2), eps = 0), "eps")
  expect_error(prep_speed(c(1, 2), eps = -1), "eps")
})



test_that("prep_travel_time per-distance + log works", {
  tt <- c(10, 20, 30)
  d <- c(5, 10, 15)

  out <- prep_travel_time(
    tt, distance = d, per_distance = TRUE,
    transform = "log", eps = 1e-6
  )

  expect_equal(out$meta$outcome, "travel_time")
  expect_true(out$meta$per_distance)
  expect_equal(out$meta$base, "travel_time_per_distance")

  bt <- out$meta$inv(out$y)
  expect_true(all(bt >= 0))
})



test_that("prep_travel_time adversarial inputs", {
  expect_error(prep_travel_time("a"), "numeric")
  expect_error(prep_travel_time(c(1, NA)), "finite")
  expect_error(prep_travel_time(c(-1, 2)), "nonnegative")
  expect_error(prep_travel_time(c(1, 2), per_distance = TRUE), "requires")
  expect_error(
    prep_travel_time(c(1, 2), distance = c(0, 1), per_distance = TRUE),
    "positive"
  )
  expect_error(prep_travel_time(c(1, 2), eps = 0), "eps")
})


test_that("fit_traffic validates required columns and dimensions", {
  A <- diag(3)
  df <- data.frame(segment_id = 1:3, speed = c(5, 6, 7))

  expect_error(
    fit_traffic(data = df[, "speed", drop = FALSE], A = A),
    "segment_id_col"
  )

  expect_error(
    fit_traffic(data = df, A = A, outcome_col = "nope"),
    "outcome_col"
  )

  expect_error(
    fit_traffic(data = df, A = A, X = matrix(1, nrow = 2, ncol = 1)),
    "nrow"
  )

  expect_error(
    fit_traffic(data = df, A = NULL, roads = NULL),
    "Provide"
  )
})


test_that("fit_traffic extreme numeric inputs do not break preprocessing", {
  df1 <- data.frame(segment_id = 1:3, speed = c(0, 1e-12, 1e12))
  out1 <- prep_speed(df1$speed, transform = "log", eps = 1e-6)
  expect_true(all(is.finite(out1$y)))

  df2 <- data.frame(
    segment_id = 1:3,
    travel_time = c(0, 1e-9, 1e9),
    dist = c(1, 2, 3)
  )
  out2 <- prep_travel_time(
    df2$travel_time, distance = df2$dist,
    per_distance = TRUE, transform = "log", eps = 1e-6
  )
  expect_true(all(is.finite(out2$y)))
})



test_that("fit_traffic minimal integration run returns traffic_fit", {
  skip_on_cran()

  A <- matrix(0, 3, 3)
  A[1,2] <- 1; A[2,1] <- 1
  A[2,3] <- 1; A[3,2] <- 1

  df <- data.frame(segment_id = 1:3, speed = c(10, 12, 11))

  set.seed(1)
  tf <- fit_traffic(
    data = df,
    A = A,
    outcome = "speed",
    transform = "log",
    X = matrix(1, 3, 1),
    type = "proper",
    rho = 0.9,
    tau = 1,
    n_iter = 40,
    burn_in = 10,
    thin = 2,
    verbose = FALSE
  )

  expect_s3_class(tf, "traffic_fit")
  expect_true(is.matrix(tf$fit$draws$x))
  expect_true(is.numeric(tf$fit$draws$sigma2))
})
