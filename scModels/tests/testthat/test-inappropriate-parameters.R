
test_that("Wrong parameters in density function", {
  expect_warning(expect_true(is.nan(dpb(0, -1, 3, 20))))
  expect_warning(expect_true(is.nan(dpb(1, -1, 3, 20))))
  expect_warning(expect_true(is.nan(dpb(1, 5, -1, 20))))
  expect_warning(expect_true(is.nan(dpb(1, 5, 3, -1))))
})

test_that("Wrong parameters in distribution function", {
  expect_warning(expect_true(is.nan(ppb(2, -1, 3, 20))))
  expect_warning(expect_true(is.nan(ppb(2, 5, -1, 20))))
  expect_warning(expect_true(is.nan(ppb(2, 5, 3, -1))))
})

test_that("Wrong parameters in quantile function", {
  expect_warning(expect_true(is.nan(qpb(-0.5, 5, 3, 20))))
  expect_warning(expect_true(is.nan(qpb(0.2, -1, 3, 20))))
  expect_warning(expect_true(is.nan(qpb(0.2, 5, -1, 20))))
  expect_warning(expect_true(is.nan(qpb(0.2, 5, 3, -1))))
})

test_that("Wrong parameters in RNG function", {
  expect_error(rpb(-1, 5, 3, 20))
  expect_warning(expect_true(is.na(rpb(1, -1, 3, 20))))
  expect_warning(expect_true(is.na(rpb(1, 5, -1, 20))))
  expect_warning(expect_true(is.na(rpb(1, 5, 3, -1))))
})
