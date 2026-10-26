test_that("probe reports NaN separately from NA", {
  m <- matrix(c(1, 2, 3, 4), 2)
  expect_false(couplr:::lap_probe_cost_matrix(m)$has_nan)

  m_nan <- m; m_nan[2] <- NaN
  expect_true(couplr:::lap_probe_cost_matrix(m_nan)$has_nan)

  m_na <- m; m_na[2] <- NA_real_
  expect_false(couplr:::lap_probe_cost_matrix(m_na)$has_nan)

  m_int <- matrix(1:4, 2); m_int[2] <- NA_integer_
  expect_false(couplr:::lap_probe_cost_matrix(m_int)$has_nan)
})

test_that("probe counts every non-finite entry", {
  m <- matrix(1:16, 4)
  storage.mode(m) <- "double"
  m[1] <- NA_real_; m[2] <- Inf; m[3] <- -Inf
  p <- couplr:::lap_probe_cost_matrix(m)
  expect_equal(p$n_nonfinite, 3)
  expect_equal(p$n_total, 16)
  expect_true(p$any_finite)

  p_none <- couplr:::lap_probe_cost_matrix(matrix(Inf, 3, 3))
  expect_equal(p_none$n_nonfinite, 9)
  expect_false(p_none$any_finite)
  expect_false(p_none$constant)
  expect_false(p_none$binary)
})

test_that("probe recognises constant and binary cost scales", {
  expect_true(couplr:::lap_probe_cost_matrix(matrix(7, 4, 4))$constant)
  expect_true(couplr:::lap_probe_cost_matrix(matrix(0, 4, 4))$constant)

  bin <- matrix(c(0, 1, 1, 0, 1, 0, 0, 1, 0), 3)
  expect_true(couplr:::lap_probe_cost_matrix(bin)$binary)
  expect_true(couplr:::lap_probe_cost_matrix(matrix(as.integer(bin), 3))$binary)

  # An intermediate value inside [0, 1] is not a binary cost scale.
  expect_false(couplr:::lap_probe_cost_matrix(
    matrix(c(0, 0.5, 1, 1, 0, 1, 0, 1, 0), 3))$binary)
  # Two distinct values that are not 0 and 1 are not either.
  expect_false(couplr:::lap_probe_cost_matrix(
    matrix(c(0, 2, 2, 0, 2, 0, 0, 2, 0), 3))$binary)
})

test_that("auto dispatch matches the documented rules", {
  pick <- function(cost) {
    assignment(cost, method = "auto")$method_used
  }
  set.seed(20260809)

  expect_equal(pick(matrix(rnorm(25), 5)), "bruteforce")
  expect_equal(pick(matrix(rnorm(64), 8)), "bruteforce")
  expect_equal(pick(matrix(rnorm(400), 20)), "jv")
  expect_equal(pick(matrix(sample(0:1, 400, TRUE), 20)), "hk01")
  expect_equal(pick(matrix(9, 20, 20)), "hk01")
  # Neither shape nor sparsity is a property the rules read, so both of these
  # fall through to the default.
  expect_equal(pick(matrix(rnorm(400), 10)), "jv")   # 10 x 40, m >= 3n

  sparse <- matrix(rnorm(400), 20)
  sparse[sample.int(400, 260)] <- Inf
  expect_equal(pick(sparse), "jv")

  half <- matrix(rnorm(400), 20)
  off <- which(row(half) != col(half))
  half[sample(off, 200)] <- Inf
  expect_equal(mean(is.infinite(half)), 0.5)
  expect_equal(pick(half), "jv")

  # Both remain reachable by name, which is what the rules gave up, not access.
  expect_equal(assignment(sparse, method = "lapmod")$method_used, "lapmod")
  expect_equal(assignment(matrix(rnorm(400), 10), method = "sap")$method_used,
               "sap")
})

test_that("dispatch is unchanged by cost storage mode", {
  set.seed(1)
  int_cost <- matrix(sample.int(1000L, 400L, replace = TRUE), 20)
  dbl_cost <- int_cost
  storage.mode(dbl_cost) <- "double"

  expect_equal(assignment(int_cost, method = "auto")$method_used,
               assignment(dbl_cost, method = "auto")$method_used)
  expect_equal(assignment(int_cost, method = "auto")$total_cost,
               assignment(dbl_cost, method = "auto")$total_cost)
})

test_that("NaN costs are still rejected for every method", {
  m <- matrix(rnorm(400), 20)
  m[7] <- NaN
  expect_error(assignment(m, method = "auto"), "NaN")
  expect_error(assignment(m, method = "jv"), "NaN")
})
