
test_that("NA parameters in density function", {
  expect_true(is.na(dpb(NA, 5, 3, 20)))
  expect_true(is.na(dpb(1, NA, 3, 20)))
  expect_true(is.na(dpb(1, 5, NA, 20)))
  expect_true(is.na(dpb(1, 5, 3, NA)))
})

test_that("NA parameters in distribution function", {
  expect_true(is.na(ppb(NA, 5, 3, 20)))
  expect_true(is.na(ppb(2, NA, 3, 20)))
  expect_true(is.na(ppb(2, 5, NA, 20)))
  expect_true(is.na(ppb(2, 5, 3, NA)))
})

test_that("NA parameters in quantile function", {
  expect_true(is.na(qpb(NA, 5, 3, 20)))
  expect_true(is.na(qpb(0.2, NA, 3, 20)))
  expect_true(is.na(qpb(0.2, 5, NA, 20)))
  expect_true(is.na(qpb(0.2, 5, 3, NA)))
})

test_that("NA parameters in RNG function", {
  expect_error(rpb(NA, 5, 3, 20))
  expect_warning(expect_true(is.na(rpb(1, NA, 3, 20))))
  expect_warning(expect_true(is.na(rpb(1, 5, NA, 20))))
  expect_warning(expect_true(is.na(rpb(1, 5, 3, NA))))
})
