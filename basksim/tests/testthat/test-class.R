test_that("setup_bma works", {
  design <- setup_bma(k = 3, p0 = 0.2)
  expect_s3_class(design, "bma")
})

test_that("setup_mml works", {
  design <- setup_mml(k = 3, p0 = 0.2)
  expect_s3_class(design, "mml")
})

test_that("setup_mmlglobal works", {
  design <- setup_mmlglobal(k = 3, p0 = 0.2)
  expect_s3_class(design, "mmlglobal")
})

test_that("setup_bhm works", {
  design <- setup_bhm(k = 3, p0 = 0.2, p_target = 0.5)
  expect_s3_class(design, "bhm")
})

test_that("setup_exnex works", {
  design <- setup_exnex(k = 3, p0 = 0.2)
  expect_s3_class(design, "exnex")
})

test_that("setup_fujikawa works", {
  design <- setup_fujikawa(k = 3, p0 = 0.2)
  expect_s3_class(design, "fujikawa")
})

test_that("setup_jsdglobal works", {
  design <- setup_jsdglobal(k = 3, p0 = 0.2)
  expect_s3_class(design, "jsdglobal")
})

test_that("setup_cpp works", {
  design <- setup_cpp(k = 3, p0 = 0.2)
  expect_s3_class(design, "cpp")
})

test_that("setup_cppglobal works", {
  design <- setup_cppglobal(k = 3, p0 = 0.2)
  expect_s3_class(design, "cppglobal")
})

test_that("errors in setup functions work", {
  expect_error(setup_fujikawa(k = 0, p0 = 0.2))
  expect_error(setup_jsdglobal(k = 3, p0 = 1))
  expect_error(setup_cpp(k = 2.5, p0 = 0.5))
  expect_error(setup_cppglobal(k = 3, p0 = 0.2, shape1 = 0, shape2 = 0))
})
