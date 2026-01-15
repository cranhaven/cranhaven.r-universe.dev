test_that("JackCombination", {
  n <- 4L
  qspray <- ESFpoly(n, c(3, 1)) + PSFpoly(n, c(2, 2))
  alpha <- "2"
  which <- "P"
  combo <- JackCombination(qspray, alpha, which)
  expect_true(qspray == JackCombinationToQspray(combo, n, alpha, which))
})

test_that("JackCombination for a non-homogeneous polynomial", {
  n <- 4L
  qspray <- ESFpoly(n, c(3, 1)) + PSFpoly(n, c(2, 1))
  alpha <- "2"
  which <- "J"
  combo <- JackCombination(qspray, alpha, which)
  expect_true(qspray == JackCombinationToQspray(combo, n, alpha, which))
})

test_that("JackCombination with alpha=1 and which='P' is Schur combination", {
  n <- 4L
  qspray <- ESFpoly(n, c(1, 1)) + PSFpoly(n, c(2, 2))
  combo <- JackCombination(qspray, alpha = "1", which = "P")
  expect_true(qspray == SchurCombinationToQspray(combo, n))
})

test_that("JackCombination for a 'degenerate' symmetric polynomial", {
  n <- 3L
  qspray <- ESFpoly(n, c(3, 1)) + PSFpoly(n, c(2, 2))
  alpha <- "3/2"
  which <- "C"
  combo <- JackCombination(qspray, alpha, which)
  expect_true(qspray == JackCombinationToQspray(combo, n, alpha, which))
})
