V <- cbind(
  c(sqrt(2) / 2, -sqrt(2) / 2, 0),
  c(sqrt(2) / 2,  sqrt(2) / 2, 0),
  c(0, 0, 1)
)
A <- V %*% diag(1:3) %*% t(V)
Id <- diag(1, 3)
Ze <- diag(0, 3)

test_that("Constructor `SPDMetricLogEuclidean()` works", {
  mt <- SPDMetricLogEuclidean$new(n = 3)
  expect_true("SPDMetricLogEuclidean" %in% class(mt))
})

test_that("Method `$inner_product()` works", {
  mt <- SPDMetricLogEuclidean$new(n = 3)
  expect_equal(mt$inner_product(Ze, Id, base_point = Id), 0)
})

test_that("Method `$squared_norm()` works", {
  mt <- SPDMetricLogEuclidean$new(n = 3)
  expect_equal(mt$squared_norm(Ze, base_point = Id), 0)
})

test_that("Method `$norm()` works", {
  mt <- SPDMetricLogEuclidean$new(n = 3)
  expect_equal(mt$norm(Ze, base_point = Id), 0)
})

test_that("Method `$squared_dist()` works", {
  mt <- SPDMetricLogEuclidean$new(n = 3)
  expect_equal(mt$squared_dist(Id, Id), 0)
})

test_that("Method `$dist()` works", {
  mt <- SPDMetricLogEuclidean$new(n = 3)
  expect_equal(mt$dist(Id, Id), 0)
})
