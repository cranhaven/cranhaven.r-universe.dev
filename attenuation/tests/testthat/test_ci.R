context("ci")

test_that("ci", {
  # The CI is disconnected.
  r = c(-0.1, sqrt(0.1), sqrt(0.1))
  N = c(1000, 1000, 10)
  CI = ci(r, N, 0.95, method = "free")
  expect_equal(class(CI), "list")
  expect_equal(length(CI), 2)

  # The CI is bouded to both sides
  r = c(.1, sqrt(0.3), sqrt(0.2))
  N = c(1000, 1000, 1000)
  CI = ci(r, N, 0.95)
  expect_gt(CI[1], -1)
  expect_lt(CI[2], 1)

  # The CI is unbounded to the right.
  r = c(0.4, sqrt(0.4), sqrt(0.4))
  N = c(1000, 1000, 1000)
  expect_equal(ci(r, N, 0.95)[2], 1)

  # The CI is unbouded to the left.
  r = c(-.3, sqrt(0.3), sqrt(0.2))
  N = c(1000, 1000, 1000)
  expect_equal(ci(r, N, 0.95)[1], -1)

  # The CI is the entire interval.
  r = c(-.1, sqrt(0.1), sqrt(0.1))
  N = c(10, 10, 10)
  expect_equal(ci(r, N, 0.95), c(-1, 1))

  # The CI is empty.
  r = c(0.8, sqrt(0.5), sqrt(0.5))
  N = c(100, 100, 100)
  expect_null(ci(r, N, 0.95))

  # The CI is empty.
  r = c(0.8, sqrt(0.5), sqrt(0.5))
  N = c(100, 100, 100)
  expect_null(ci(r, N, 0.95, method = "HS"))

  # The CI unbounded to the right.
  r = c(0.5, sqrt(0.5), sqrt(0.5))
  N = c(100, 100, 100)
  expect_equal(ci(r, N, 0.95, method = "HS")[2],
               1)
})

test_that("cc", {
  r = c(-0.1, sqrt(0.1), sqrt(0.1))
  N = c(1000, 1000, 10)
  rho = seq(-1, 1, by = 0.1)
  expect_equal(as.numeric(cc(r, N, by = 0.1)),
               as.numeric(1 - p_value(rho, r, N)))

  # Cronbach
  r = c(-0.1, sqrt(0.1), sqrt(0.1))
  k = c(4, 4)
  N = c(1000, 1000, 10)
  rho = seq(-1, 1, by = 0.2)
  expect_equal(as.numeric(cc(r, N, method = "cronbach", k = k, by = 0.2)),
               as.numeric(1 - p_value(rho, r, N, method = "cronbach", k = k)))

  # HS
  r = c(-0.1, sqrt(0.1), sqrt(0.1))
  N = c(1000, 1000, 10)
  rho = seq(-1, 1, by = 0.2)
  expect_equal(as.numeric(cc(r, N, method = "HS", by = 0.2)),
               as.numeric(1 - p_value(rho, r, N, method = "HS", k = k)))

})
