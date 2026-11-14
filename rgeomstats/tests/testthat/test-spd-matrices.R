V <- cbind(
  c(sqrt(2) / 2, -sqrt(2) / 2, 0),
  c(sqrt(2) / 2,  sqrt(2) / 2, 0),
  c(0, 0, 1)
)
A <- V %*% diag(1:3) %*% t(V)
B <- V %*% diag(4:6) %*% t(V)
Id <- diag(1, 3)
Ze <- diag(0, 3)
S1 <- array(dim = c(2, 3, 3))
S1[1, , ] <- A
S1[2, , ] <- B
S2 <- array(dim = c(3, 3, 3))
S2[1, , ] <- A
S2[2, , ] <- B
S2[3, , ] <- Id

test_that("SPDMatrices constructor works", {
  spd3 <- SPDMatrix(n = 3)
  expect_true("SPDMatrices" %in% class(spd3))
})

test_that("SPDMatrices method cholesky_factor() works", {
  spd3 <- SPDMatrix(n = 3)
  expect_snapshot(spd3$cholesky_factor(A))
})

test_that("SPDMatrices method differential_cholesky_factor() works", {
  spd3 <- SPDMatrix(n = 3)
  expect_snapshot(spd3$differential_cholesky_factor(Id, A))
})

test_that("SPDMatrices method expm() works", {
  spd3 <- SPDMatrix(n = 3)
  expect_equal(spd3$expm(Ze), Id)
})

test_that("SPDMatrices method differential_exp() works", {
  spd3 <- SPDMatrix(n = 3)
  expect_snapshot(spd3$differential_exp(Id, A))
})

test_that("SPDMatrices method inverse_differential_exp() works", {
  spd3 <- SPDMatrix(n = 3)
  expect_snapshot(spd3$inverse_differential_exp(Id, A))
})

test_that("SPDMatrices method logm() works", {
  spd3 <- SPDMatrix(n = 3)
  expect_equal(spd3$logm(Id), Ze)
})

test_that("SPDMatrices method differential_log() works", {
  spd3 <- SPDMatrix(n = 3)
  expect_snapshot(spd3$differential_log(Id, A))
})

test_that("SPDMatrices method inverse_differential_log() works", {
  spd3 <- SPDMatrix(n = 3)
  expect_snapshot(spd3$inverse_differential_log(Id, A))
})

test_that("SPDMatrices method powerm() works", {
  spd3 <- SPDMatrix(n = 3)
  expect_equal(spd3$powerm(Id, 2), Id)
})

test_that("SPDMatrices method differential_power() works", {
  spd3 <- SPDMatrix(n = 3)
  expect_snapshot(spd3$differential_power(2, Id, A))
})

test_that("SPDMatrices method inverse_differential_power() works", {
  spd3 <- SPDMatrix(n = 3)
  expect_snapshot(spd3$inverse_differential_power(2, Id, A))
})

test_that("SPDMatrices method projection() works", {
  spd3 <- SPDMatrix(n = 3)
  expect_equal(spd3$projection(Id), Id)
})

test_that("SPDMatrices method belongs() works", {
  spd3 <- SPDMatrix(n = 3)
  expect_true(spd3$belongs(A))
  expect_false(spd3$belongs(diag(-1, 3)))
})

test_that("SPDMatrices method is_tangent() works", {
  spd3 <- SPDMatrix(n = 3)
  expect_true(spd3$is_tangent(Ze))
})

test_that("SPDMatrices method to_tangent() works", {
  spd3 <- SPDMatrix(n = 3)
  expect_equal(spd3$to_tangent(Id), Id) # TO DO: not sure I understand why Id projects to Id.
})

test_that("SPDMatrices method random_point() works", {
  skip("Needs bug fix in gs")
  reticulate::py_set_seed(1234)
  spd3 <- SPDMatrix(n = 3)
  expect_snapshot(spd3$random_point(10))
})

test_that("SPDMatrices method regularize() works", {
  spd3 <- SPDMatrix(n = 3)
  expect_equal(spd3$regularize(Id), Id)
})

test_that("SPDMatrices method set_metric() works", {
  spd3 <- SPDMatrix(n = 3)
  met <- SPDMetricBuresWasserstein$new(n = 3)
  spd3$set_metric(met)
  expect_equal(spd3$metric, met$get_python_class())
})

test_that("SPDMatrices method random_tangent_vec() works", {
  reticulate::py_set_seed(1234)
  spd3 <- SPDMatrix(n = 3)
  expect_snapshot(spd3$random_tangent_vec(Id, 10))
})
