context("Main functions")

test_that("maxnodf",{
  m <- matrix(0,5,5)
  diag(m) <- 1
  m[1:2,] <- 1
  expect_equal(maxnodf(m, quality = 0)$max_nodf, 0.9)
  expect_equal(maxnodf(m, quality = 1)$max_nodf, 0.9)
  expect_equal(maxnodf(m, quality = 2)$max_nodf, 0.9)
  expect_equal(maxnodf(c(5,5,13), quality = 0)$max_nodf, 0.9)
})

test_that("Errors and warnings",{
  m <- matrix(0,5,5)
  diag(m) <- 1
  expect_warning(maxnodf(m, quality = 0)$max_nodf)
  expect_error(maxnodf(c(1,1)))
  expect_error(maxnodf(matrix(0,10,10)))
  expect_error(maxnodf(matrix(-1,10,10)))
  m <- matrix(0,5,5)
  diag(m) <- 1
  m[1:2,] <- 1
  expect_error(maxnodf(m,quality = 4))
})


test_that("NODFc",{
  m <- matrix(0,5,5)
  diag(m) <- 1
  m[1:2,] <- 1
  expect_equal(round(NODFc(m, quality = 0),6), 1.834201)
  expect_equal(round(NODFc(m, quality = 1),6), 1.834201)
  expect_equal(round(NODFc(m, quality = 2),6), 1.834201)
})