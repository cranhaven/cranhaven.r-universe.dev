library(pipenostics)

test_that("*inch_mm* errs in calculation", {
  expect_equal(
    inch_mm(c(25.4, 1)),
    c(1.00000000, 0.03937008),
    tolerance = 1e-7
  )
})

test_that("*mm_inch* errs in calculation", {
  expect_equal(
    mm_inch(c(0.03937008, 1)),
    c(1.0, 25.4),
    tolerance = 1e-2
  )
})

test_that("*kgf_mpa* errs in calculation", {
  expect_equal(
    kgf_mpa(c(0.0980665, 1)),
    c(1.00000, 10.19716),
    tolerance = 1e-5
  )
})


test_that("*mpa_kgf* errs in calculation", {
  expect_equal(
    mpa_kgf(c(10.1971619998, 1)),
    c(1.0000000, 0.0980665),
    tolerance = 1e-6
  )
})


test_that("*mpa_psi* errs in calculation", {
  expect_equal(
    mpa_psi(c(145.03773800721814, 1)),
    c(1.000000000, 0.006894757),
    tolerance = 1e-7
  )
})


test_that("*psi_mpa* errs in calculation", {
  expect_equal(
    psi_mpa(c(6.89475728e-3, 1)),
    c(1.0000, 145.0377),
    tolerance = 1e-5
  )
})


test_that("*k_c* errs in calculation", {
  expect_equal(
   k_c(c(-273.15, 100)),
   c(0, 373.15),
   tolerance = 1e-8
  )
})


test_that("*k_f* errs in calculation", {
  expect_equal(
    k_f(c(-459.67, 212)),
    c(0, 373.15),
    tolerance = 1e-8
  )
})


test_that("*c_k* errs in calculation", {
  expect_equal(
    c_k(c(0, 373.15)),
    c(-273.15, 100),
    tolerance = 1e-8
  )
})


test_that("*c_f* errs in calculation", {
  expect_equal(
    c_f(c(-459.67, 212)),
    c(-273.15, 100),
    tolerance = 1e-8
  )
})


test_that("*f_k* errs in calculation", {
  expect_equal(
    f_k(c(0, 373.15)),
    c(-459.67, 212),
    tolerance = 1e-8
  )
})


test_that("*f_c* errs in calculation", {
  expect_equal(
    f_c(c(-273.15, 100)),
    c(-459.67, 212),
    tolerance = 1e-8
  )
})


test_that("*flux_loss* errs in calculation", {
  expect_equal(
    flux_loss(c(218, 1040), c(998, 1395) * 1e-3, c(2, 5)),
    c(80.70238, 275.00155),
    tolerance = 5e-6
  )
})


test_that("*loss_flux* errs in calculation", {
  expect_equal(
    loss_flux(c(80.70238, 275.00155), c(998, 1395) * 1e-3, c(2, 5)),
    c(218, 1040),
    tolerance = 5e-6
  )
})
