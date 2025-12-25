test_that("Test kluster handles missing values appropriately", {
  expect_error(kluster(NA))
  expect_equal(kluster(c(10, NA)), c(1, NA))
  expect_equal(is.na(kluster(c(100, 0, NA))), is.na(c(100, 0, NA)))
  expect_error(kluster(integer()))
})

test_that("Test kluster behavior for various input types", {
  expect_equal(kluster(Sys.time()), 1)
  expect_error(kluster("a"))
})

test_that("Test kluster provides expected output", {
  expect_equal(unique(kluster(mopac::rush_hour$time)), 1:7)
  expect_equal(kluster(1e8 + 0:5), kluster(0:5))
})

