test_that("trace works", {
  mat <- matrix(2, 3, 3)
  expect_equal(tr(mat), 6)
})

test_that("thurstone works", {
  lambda <- sigma <- rep(1, 10)
  expect_equal(sum(thurstone(lambda, sigma)), 10 / 11)
})

test_that("covmat works", {
  lambda <- sigma <- rep(1, 10)
  expect_equal(covmat(lambda, sigma), lambda %*% t(lambda) + diag(sigma^2))
})
