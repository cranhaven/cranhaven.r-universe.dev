# ==============================================================================
# Exact arithmetic on sums of doubles
# ==============================================================================
# The branch and bound of cardinality_match() compares bounds, objectives and
# moment rows exactly. Each case below is one the double evaluation of the same
# expression answers wrongly, so a routine that quietly fell back to doubles
# would fail it.
# ==============================================================================

test_that("a sum the doubles round is compared exactly", {
  # 0.1 + 0.2 as doubles sums to a value strictly between 0.3 and the double
  # 0.30000000000000004 that fl(0.1 + 0.2) rounds it to.
  s <- lap_exact_dot(c(0.1, 0.2), c(1, 1))
  expect_identical(lap_exact_compare(s, 0.3), 1L)
  expect_identical(lap_exact_compare(s, 0.1 + 0.2), -1L)
  expect_identical(lap_exact_compare(s, s), 0L)
  expect_identical(lap_exact_round(s, "down"), 0.3)
  expect_identical(lap_exact_round(s, "up"), 0.1 + 0.2)
})

test_that("a value a double holds rounds to itself both ways", {
  for (x in c(0, 1, -2.5, 1e300, 2^-1074)) {
    e <- lap_exact_dot(x, 1)
    expect_identical(lap_exact_round(e, "down"), x)
    expect_identical(lap_exact_round(e, "up"), x)
  }
})

test_that("a product is held exactly", {
  # (1 + 2^-30)^2 = 1 + 2^-29 + 2^-60, which no double holds.
  a <- 1 + 2^-30
  p <- lap_exact_dot(a, a)
  expect_identical(lap_exact_compare(p, a * a), 1L)
  expect_identical(lap_exact_compare(p, lap_exact_dot(c(1, 2^-29, 2^-60),
                                                      c(1, 1, 1))), 0L)
})

test_that("the ceiling of a quotient is decided exactly", {
  expect_identical(lap_exact_ceil_quotient(6, 0, 3), 2)
  expect_identical(lap_exact_ceil_quotient(lap_exact_dot(c(6, 2^-50), c(1, 1)),
                                           0, 3), 3)
  expect_identical(lap_exact_ceil_quotient(lap_exact_dot(c(6, -2^-50), c(1, 1)),
                                           0, 3), 2)
  expect_identical(lap_exact_ceil_quotient(-7, 1, 3), -2)
  expect_identical(lap_exact_ceil_quotient(numeric(0), 0, 5), 0)
})

test_that("a moment row the doubles call satisfied is refused", {
  # Two pairs with u = 1 and u = 1e-17 against b = 0.5: the doubles sum the
  # row to exactly zero, and it is 1e-17 above.
  u <- matrix(c(1, 1e-17), ncol = 1L)
  w <- matrix(c(0, 0), ncol = 1L)
  rows <- lap_exact_moment_rows(u, w, 0.5, 1:2, 1:2, 0)
  expect_identical(sum(u) - sum(w) - 2 * 0.5, 0)
  expect_identical(rows$sign, 1L)

  rows <- lap_exact_moment_rows(u, w, 0.5, 1L, 1L, 0)
  expect_identical(rows$sign, 1L)
  rows <- lap_exact_moment_rows(u, w, 0.5, integer(0), integer(0), 0)
  expect_identical(rows$sign, 0L)
})

test_that("the multiplier-weighted sum takes the sign of the rows it weights", {
  u <- matrix(c(1, 0, 0, 1), 2L)
  w <- matrix(0, 2L, 2L)
  b <- c(0.25, 2)
  rows <- lap_exact_moment_rows(u, w, b, 1:2, 1:2, c(0, 0))
  expect_identical(rows$sign, c(1L, -1L))
  expect_identical(rows$weighted_sign, 0L)
  expect_identical(lap_exact_moment_rows(u, w, b, 1:2, 1:2, c(1, 0))$weighted_sign, 1L)
  expect_identical(lap_exact_moment_rows(u, w, b, 1:2, 1:2, c(0, 1))$weighted_sign, -1L)
})
