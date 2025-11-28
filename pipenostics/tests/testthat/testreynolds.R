library(pipenostics)

test_that("re_u function errs in estimating Reynolds number", {
  expect_equal(
    re_u(c(.25, 1), .89, 1, 1000),
    c(280.8989, 1123.5955),
    tolerance = 1e-4
  )
})

test_that("re_v function errs in estimating Reynolds number", {
  expect_equal(
    re_v(c(.25, 1), c(.89, .89), c(0.04908739, 0.78539816), c(1000, 1000)),
    c(280.8989, 1123.5955),
    tolerance = 1e-4
  )
})

test_that("re_m function errs in estimating Reynolds number", {
  expect_equal(
    re_m(c(.25, 1), c(.89, .89), c(49.08739, 785.39816)),
    c(280.8989, 1123.5955),
    tolerance = 1e-4
  )
})
