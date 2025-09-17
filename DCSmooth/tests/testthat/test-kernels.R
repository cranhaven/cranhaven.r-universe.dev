################################################################################
#                                                                              #
#                               Test Kernels                                   #
#                                                                              #
################################################################################


# Müller-Wang Kernels
context("test MW kernels")

test_that("0 moments are correct.", {
  n_test = 250000.5
  u = seq(from = -1, to = 1, length.out = 2*n_test)
  MW200 = kernel.assign("MW_200")
  MW210 = kernel.assign("MW_210")
  MW220 = kernel.assign("MW_220")
  MW321 = kernel.assign("MW_321")
  MW420 = kernel.assign("MW_420")
  MW421 = kernel.assign("MW_421")
  MW422 = kernel.assign("MW_422")
  MW533 = kernel.assign("MW_533")
  MW644 = kernel.assign("MW_644")
  
  expect_equal(sum(MW200(u, 1))/n_test, 1, tolerance = 1e-04)
  expect_equal(sum(MW210(u, 1))/n_test, 1, tolerance = 1e-04)
  expect_equal(sum(MW220(u, 1))/n_test, 1, tolerance = 1e-04)
  expect_equal(sum(MW321(u, 1))/n_test, 0, tolerance = 1e-04)
  expect_equal(sum(MW420(u, 1))/n_test, 1, tolerance = 1e-04)
  expect_equal(sum(MW421(u, 1))/n_test, 0, tolerance = 1e-04)
  expect_equal(sum(MW422(u, 1))/n_test, 0, tolerance = 1e-04)
  expect_equal(sum(MW533(u, 1))/n_test, 0, tolerance = 1e-04)
  expect_equal(sum(MW644(u, 1))/n_test, 0, tolerance = 1e-04)
})

test_that("1st moments are correct.", {
  n_test = 250000.5
  u = seq(from = -1, to = 1, length.out = 2*n_test)
  MW200 = kernel.assign("MW_200")
  MW210 = kernel.assign("MW_210")
  MW220 = kernel.assign("MW_220")
  MW321 = kernel.assign("MW_321")
  MW420 = kernel.assign("MW_420")
  MW421 = kernel.assign("MW_421")
  MW422 = kernel.assign("MW_422")
  MW533 = kernel.assign("MW_533")
  MW644 = kernel.assign("MW_644")
  
  expect_equal(sum(MW200(u, 1) * u)/n_test, 0, tolerance = 1e-04)
  expect_equal(sum(MW210(u, 1) * u)/n_test, 0, tolerance = 1e-04)
  expect_equal(sum(MW220(u, 1) * u)/n_test, 0, tolerance = 1e-04)
  expect_equal(sum(MW321(u, 1) * u)/n_test, -1, tolerance = 1e-04)
  expect_equal(sum(MW420(u, 1) * u)/n_test, 0, tolerance = 1e-04)
  expect_equal(sum(MW421(u, 1) * u)/n_test, -1, tolerance = 1e-04)
  expect_equal(sum(MW422(u, 1) * u)/n_test, 0, tolerance = 1e-04)
  expect_equal(sum(MW533(u, 1) * u)/n_test, 0, tolerance = 1e-04)
  expect_equal(sum(MW644(u, 1) * u)/n_test, 0, tolerance = 1e-04)
})

test_that("2nd moments are correct.", {
  n_test = 250000.5
  u = seq(from = -1, to = 1, length.out = 2*n_test)
  MW200 = kernel.assign("MW_200")
  MW210 = kernel.assign("MW_210")
  MW220 = kernel.assign("MW_220")
  MW321 = kernel.assign("MW_321")
  MW420 = kernel.assign("MW_420")
  MW421 = kernel.assign("MW_421")
  MW422 = kernel.assign("MW_422")
  MW533 = kernel.assign("MW_533")
  MW644 = kernel.assign("MW_644")
  
  expect_equal(sum(MW200(u, 1) * u^2)/n_test, 1/3, tolerance = 1e-04)
  expect_equal(sum(MW210(u, 1) * u^2)/n_test, 1/5, tolerance = 1e-04)
  expect_equal(sum(MW220(u, 1) * u^2)/n_test, 1/7, tolerance = 1e-04)
  expect_equal(sum(MW321(u, 1) * u^2)/n_test, 0, tolerance = 1e-04)
  expect_equal(sum(MW420(u, 1) * u^2)/n_test, 0, tolerance = 1e-04)
  expect_equal(sum(MW421(u, 1) * u^2)/n_test, 0, tolerance = 1e-04)
  expect_equal(sum(MW422(u, 1) * u^2)/n_test, 2, tolerance = 1e-04)
  expect_equal(sum(MW533(u, 1) * u^2)/n_test, 0, tolerance = 1e-04)
  expect_equal(sum(MW644(u, 1) * u^2)/n_test, 0, tolerance = 1e-04)
})

test_that("3rd moments are correct.", {
  n_test = 250000.5
  u = seq(from = -1, to = 1, length.out = 2*n_test)
  MW533 = kernel.assign("MW_533")
  
  expect_equal(sum(MW533(u, 1) * u^3)/n_test, -6, tolerance = 1e-04)
})

test_that("4th moments are correct.", {
  n_test = 250000.5
  u = seq(from = -1, to = 1, length.out = 2*n_test)
  MW420 = kernel.assign("MW_420")
  MW421 = kernel.assign("MW_421")
  MW422 = kernel.assign("MW_422")
  MW533 = kernel.assign("MW_533")
  MW644 = kernel.assign("MW_644")
  
  expect_equal(sum(MW420(u, 1) * u^4)/n_test, -1/33, tolerance = 1e-04)
  expect_equal(sum(MW421(u, 1) * u^4)/n_test, 4/33, tolerance = 1e-04)
  expect_equal(sum(MW422(u, 1) * u^4)/n_test, 12/11, tolerance = 1e-04)
  expect_equal(sum(MW533(u, 1) * u^4)/n_test, 0, tolerance = 1e-04)
  expect_equal(sum(MW644(u, 1) * u^4)/n_test, 24, tolerance = 1e-04)
})

### Müller Kernels
context("test M kernels")

test_that("0 moments are correct.", {
  n_test = 250000.5
  u = seq(from = -1, to = 1, length.out = 2*n_test)
  M200 = kernel.assign("M_200")
  M210 = kernel.assign("M_210")
  M220 = kernel.assign("M_220")
  M321 = kernel.assign("M_321")
  M420 = kernel.assign("M_420")
  M421 = kernel.assign("M_421")
  M422 = kernel.assign("M_422")
  
  expect_equal(sum(M200(u, 1))/n_test, 1, tolerance = 1e-04)
  expect_equal(sum(M210(u, 1))/n_test, 1, tolerance = 1e-04)
  expect_equal(sum(M220(u, 1))/n_test, 1, tolerance = 1e-04)
  expect_equal(sum(M321(u, 1))/n_test, 0, tolerance = 1e-04)
  expect_equal(sum(M420(u, 1))/n_test, 1, tolerance = 1e-04)
  expect_equal(sum(M421(u, 1))/n_test, 0, tolerance = 1e-04)
  expect_equal(sum(M422(u, 1))/n_test, 0, tolerance = 1e-04)
})

test_that("1st moments are correct.", {
  n_test = 250000.5
  u = seq(from = -1, to = 1, length.out = 2*n_test)
  M200 = kernel.assign("M_200")
  M210 = kernel.assign("M_210")
  M220 = kernel.assign("M_220")
  M321 = kernel.assign("M_321")
  M420 = kernel.assign("M_420")
  M421 = kernel.assign("M_421")
  M422 = kernel.assign("M_422")
  
  expect_equal(sum(M200(u, 1) * u)/n_test, 0, tolerance = 1e-04)
  expect_equal(sum(M210(u, 1) * u)/n_test, 0, tolerance = 1e-04)
  expect_equal(sum(M220(u, 1) * u)/n_test, 0, tolerance = 1e-04)
  expect_equal(sum(M321(u, 1) * u)/n_test, -1, tolerance = 1e-04)
  expect_equal(sum(M420(u, 1) * u)/n_test, 0, tolerance = 1e-04)
  expect_equal(sum(M421(u, 1) * u)/n_test, -1, tolerance = 1e-04)
  expect_equal(sum(M422(u, 1) * u)/n_test, 0, tolerance = 1e-04)
})

test_that("2nd moments are correct.", {
  n_test = 250000.5
  u = seq(from = -1, to = 1, length.out = 2*n_test)
  M200 = kernel.assign("M_200")
  M210 = kernel.assign("M_210")
  M220 = kernel.assign("M_220")
  M321 = kernel.assign("M_321")
  M420 = kernel.assign("M_420")
  M421 = kernel.assign("M_421")
  M422 = kernel.assign("M_422")
  
  expect_equal(sum(M200(u, 1) * u^2)/n_test, 1/3, tolerance = 1e-04)
  expect_equal(sum(M210(u, 1) * u^2)/n_test, 1/5, tolerance = 1e-04)
  expect_equal(sum(M220(u, 1) * u^2)/n_test, 1/7, tolerance = 1e-04)
  expect_equal(sum(M321(u, 1) * u^2)/n_test, 0, tolerance = 1e-04)
  expect_equal(sum(M420(u, 1) * u^2)/n_test, 0, tolerance = 1e-04)
  expect_equal(sum(M421(u, 1) * u^2)/n_test, 0, tolerance = 1e-04)
  expect_equal(sum(M422(u, 1) * u^2)/n_test, 2, tolerance = 1e-04)
})

test_that("3rd moments are correct.", {
  n_test = 250000.5
  u = seq(from = -1, to = 1, length.out = 2*n_test)
  MW533 = kernel.assign("M_533")
  
  expect_equal(sum(MW533(u, 1) * u^3)/n_test, -6, tolerance = 1e-04)
})

test_that("4th moments are correct.", {
  n_test = 250000.5
  u = seq(from = -1, to = 1, length.out = 2*n_test)
  M420 = kernel.assign("M_420")
  M421 = kernel.assign("M_421")
  M422 = kernel.assign("M_422")
  
  expect_equal(sum(M420(u, 1) * u^4)/n_test, -1/33, tolerance = 1e-04)
  expect_equal(sum(M421(u, 1) * u^4)/n_test, 0, tolerance = 1e-04)
  expect_equal(sum(M422(u, 1) * u^4)/n_test, 12/11, tolerance = 1e-04)
})


### Truncated Kernels
context("test T kernels")

test_that("0 moments are correct.", {
  n_test = 250000.5
  u = seq(from = -1, to = 1, length.out = 2*n_test)
  T220 = kernel.assign("T_220")
  T321 = kernel.assign("T_321")
  T420 = kernel.assign("T_420")
  T422 = kernel.assign("T_422")
  
  expect_equal(sum(T220(u, 1))/n_test, 1, tolerance = 1e-04)
  expect_equal(sum(T321(u, 1))/n_test, 0, tolerance = 1e-04)
  expect_equal(sum(T420(u, 1))/n_test, 1, tolerance = 1e-04)
  expect_equal(sum(T422(u, 1))/n_test, 0, tolerance = 1e-04)
})

test_that("1st moments are correct.", {
  n_test = 250000.5
  u = seq(from = -1, to = 1, length.out = 2*n_test)
  T220 = kernel.assign("T_220")
  T321 = kernel.assign("T_321")
  T420 = kernel.assign("T_420")
  T422 = kernel.assign("T_422")
  
  expect_equal(sum(T220(u, 1) * u)/n_test, 0, tolerance = 1e-04)
  expect_equal(sum(T321(u, 1) * u)/n_test, -1, tolerance = 1e-04)
  expect_equal(sum(T420(u, 1) * u)/n_test, 0, tolerance = 1e-04)
  expect_equal(sum(T422(u, 1) * u)/n_test, 0, tolerance = 1e-04)
})

test_that("2nd moments are correct.", {
  n_test = 250000.5
  u = seq(from = -1, to = 1, length.out = 2*n_test)
  T220 = kernel.assign("T_220")
  T321 = kernel.assign("T_321")
  T420 = kernel.assign("T_420")
  T422 = kernel.assign("T_422")
  
  expect_equal(sum(T220(u, 1) * u^2)/n_test, 1/7, tolerance = 1e-04)
  expect_equal(sum(T321(u, 1) * u^2)/n_test, 0, tolerance = 1e-04)
  expect_equal(sum(T420(u, 1) * u^2)/n_test, 0, tolerance = 1e-04)
  expect_equal(sum(T422(u, 1) * u^2)/n_test, 2, tolerance = 1e-04)
})

test_that("4th moments are correct.", {
  n_test = 250000.5
  u = seq(from = -1, to = 1, length.out = 2*n_test)
  T420 = kernel.assign("T_420")
  T422 = kernel.assign("T_422")
  
  expect_equal(sum(T420(u, 1) * u^4)/n_test, -1/33, tolerance = 1e-04)
  expect_equal(sum(T422(u, 1) * u^4)/n_test, 12/11, tolerance = 1e-04)
})