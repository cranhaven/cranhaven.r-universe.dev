test_that("fftw_r2c_2d", {
  # even margins
  x <- array(rnorm(1000), c(20,50))

  a <- fftwtools::fftw_r2c_2d(x, HermConj = 1)
  b <- ravetools:::fftw_r2c_2d(x, HermConj = 1)
  testthat::expect_equal(b, a, tolerance = 1e-10)

  a <- fftwtools::fftw_r2c_2d(x, HermConj = 0)
  b <- ravetools:::fftw_r2c_2d(x, HermConj = 0)
  testthat::expect_equal(b, a, tolerance = 1e-10)

  dim(x) <- c(1, 1000)
  a <- fftwtools::fftw_r2c_2d(x, HermConj = 1)
  b <- ravetools:::fftw_r2c_2d(x, HermConj = 1)
  testthat::expect_equal(b, a, tolerance = 1e-10)

  a <- fftwtools::fftw_r2c_2d(x, HermConj = 0)
  b <- ravetools:::fftw_r2c_2d(x, HermConj = 0)
  testthat::expect_equal(b, a, tolerance = 1e-10)

  dim(x) <- c(1000, 1)
  a <- t(fftwtools::fftw_r2c_2d(t(x), HermConj = 1))
  b <- ravetools:::fftw_r2c_2d(x, HermConj = 1)
  testthat::expect_equal(b, a, tolerance = 1e-10)

  a <- t(fftwtools::fftw_r2c_2d(t(x), HermConj = 0))[1:501,1, drop = FALSE]
  b <- ravetools:::fftw_r2c_2d(x, HermConj = 0)
  testthat::expect_equal(b, a, tolerance = 1e-10)


  # odd
  x <- array(rnorm(1071), c(21,51))

  a <- fftwtools::fftw_r2c_2d(x, HermConj = 1)
  b <- ravetools:::fftw_r2c_2d(x, HermConj = 1)
  testthat::expect_equal(b, a, tolerance = 1e-10)

  a <- fftwtools::fftw_r2c_2d(x, HermConj = 0)
  b <- ravetools:::fftw_r2c_2d(x, HermConj = 0)
  testthat::expect_equal(b, a, tolerance = 1e-10)

  dim(x) <- c(1, 1071)
  a <- fftwtools::fftw_r2c_2d(x, HermConj = 1)
  b <- ravetools:::fftw_r2c_2d(x, HermConj = 1)
  testthat::expect_equal(b, a, tolerance = 1e-10)

  a <- fftwtools::fftw_r2c_2d(x, HermConj = 0)
  b <- ravetools:::fftw_r2c_2d(x, HermConj = 0)
  testthat::expect_equal(b, a, tolerance = 1e-10)

  dim(x) <- c(1071, 1)
  a <- t(fftwtools::fftw_r2c_2d(t(x), HermConj = 1))
  b <- ravetools:::fftw_r2c_2d(x, HermConj = 1)
  testthat::expect_equal(b, a, tolerance = 1e-10)

  a <- t(fftwtools::fftw_r2c_2d(t(x), HermConj = 0))[1:536,1, drop = FALSE]
  b <- ravetools:::fftw_r2c_2d(x, HermConj = 0)
  testthat::expect_equal(b, a, tolerance = 1e-10)

  # zero length/margin
  x <- numeric(0)
  dim(x) <- c(0, 100)

  b <- ravetools:::fftw_r2c_2d(x, HermConj = 1)
  testthat::expect_equal(dim(b), dim(x))

  b <- ravetools:::fftw_r2c_2d(x, HermConj = 0)
  testthat::expect_equal(dim(b), dim(x))

  dim(x) <- c(100, 0)
  b <- ravetools:::fftw_r2c_2d(x, HermConj = 1)
  testthat::expect_equal(dim(b), dim(x))

  b <- ravetools:::fftw_r2c_2d(x, HermConj = 0)
  testthat::expect_equal(dim(b), c(51, 0))

})



test_that("fftw_c2c_2d", {
  # even margins
  x <- array(rnorm(1000), c(20,50))

  a <- fftwtools::fftw_c2c_2d(x, inverse = 1)
  b <- ravetools:::fftw_c2c_2d(x, inverse = 1)
  testthat::expect_equal(b, a, tolerance = 1e-10)

  a <- fftwtools::fftw_c2c_2d(x, inverse = 0)
  b <- ravetools:::fftw_c2c_2d(x, inverse = 0)
  testthat::expect_equal(b, a, tolerance = 1e-10)

  dim(x) <- c(1, 1000)
  a <- fftwtools::fftw_c2c_2d(x, inverse = 1)
  b <- ravetools:::fftw_c2c_2d(x, inverse = 1)
  testthat::expect_equal(b, a, tolerance = 1e-10)

  a <- fftwtools::fftw_c2c_2d(x, inverse = 0)
  b <- ravetools:::fftw_c2c_2d(x, inverse = 0)
  testthat::expect_equal(b, a, tolerance = 1e-10)

  dim(x) <- c(1000, 1)
  a <- fftwtools::fftw_c2c_2d(x, inverse = 1)
  b <- ravetools:::fftw_c2c_2d(x, inverse = 1)
  testthat::expect_equal(b, a, tolerance = 1e-10)

  a <- fftwtools::fftw_c2c_2d(x, inverse = 0)
  b <- ravetools:::fftw_c2c_2d(x, inverse = 0)
  testthat::expect_equal(b, a, tolerance = 1e-10)


  # odd
  x <- array(rnorm(1071), c(21,51))

  a <- fftwtools::fftw_c2c_2d(x, inverse = 1)
  b <- ravetools:::fftw_c2c_2d(x, inverse = 1)
  testthat::expect_equal(b, a, tolerance = 1e-10)

  a <- fftwtools::fftw_c2c_2d(x, inverse = 0)
  b <- ravetools:::fftw_c2c_2d(x, inverse = 0)
  testthat::expect_equal(b, a, tolerance = 1e-10)

  dim(x) <- c(1, 1071)
  a <- fftwtools::fftw_c2c_2d(x, inverse = 1)
  b <- ravetools:::fftw_c2c_2d(x, inverse = 1)
  testthat::expect_equal(b, a, tolerance = 1e-10)

  a <- fftwtools::fftw_c2c_2d(x, inverse = 0)
  b <- ravetools:::fftw_c2c_2d(x, inverse = 0)
  testthat::expect_equal(b, a, tolerance = 1e-10)

  dim(x) <- c(1071, 1)
  a <- fftwtools::fftw_c2c_2d(x, inverse = 1)
  b <- ravetools:::fftw_c2c_2d(x, inverse = 1)
  testthat::expect_equal(b, a, tolerance = 1e-10)

  a <- fftwtools::fftw_c2c_2d(x, inverse = 0)
  b <- ravetools:::fftw_c2c_2d(x, inverse = 0)
  testthat::expect_equal(b, a, tolerance = 1e-10)

  # zero length/margin
  x <- numeric(0)
  dim(x) <- c(0, 100)

  b <- ravetools:::fftw_c2c_2d(x, inverse = 1)
  testthat::expect_equal(dim(b), dim(x))

  b <- ravetools:::fftw_c2c_2d(x, inverse = 0)
  testthat::expect_equal(dim(b), dim(x))

  dim(x) <- c(100, 0)
  b <- ravetools:::fftw_c2c_2d(x, inverse = 1)
  testthat::expect_equal(dim(b), dim(x))

  b <- ravetools:::fftw_c2c_2d(x, inverse = 0)
  testthat::expect_equal(dim(b), dim(x))

})
