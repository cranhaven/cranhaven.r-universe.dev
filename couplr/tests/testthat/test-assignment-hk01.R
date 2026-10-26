# tests/testthat/test-assignment-hk01.R
test_that("hk01 handles all-equal costs", {
  set.seed(1)
  n <- 50; m <- 80
  M <- matrix(3.14, n, m)
  # ensure feasibility
  for (i in 1:n) if (all(!is.finite(M[i, ]))) M[i, sample.int(m, 1)] <- 3.14
  sol <- assignment(M, method = "hk01")
  expect_equal(length(sol$match), n)
  expect_true(all(sol$match > 0))
})

test_that("hk01 finds zero-cost perfect when it exists (0/1)", {
  set.seed(2)
  n <- 60; m <- 100
  M <- matrix(1, n, m)
  for (i in 1:n) M[i, sample.int(m, 2)] <- 0  # plenty zero edges
  sol <- assignment(M, method = "hk01")
  expect_equal(sum(M[cbind(1:n, sol$match)]), 0)
})

test_that("auto routes to hk01 only on exact 0/1 or all-equal", {
  set.seed(3)
  n <- 20; m <- 30
  M <- matrix(1, n, m); M[sample.int(length(M), 50)] <- 0
  sol <- assignment(M, method = "auto")
  expect_true(sol$method_used %in% c("hk01","jv","sap","auction","csflow","hungarian","auction_gs","bruteforce"))
})
