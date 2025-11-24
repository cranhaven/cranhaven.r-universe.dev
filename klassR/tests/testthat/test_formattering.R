test_that("Formatting works for nace codes of differnt levels", {
  x <- c("01", "011", "0110", "01110")
  suppressWarnings(codes <- klassR:::formattering(x, classification = 6))
  expect_equal(codes, c("01", "01.1", "01.10", "01.110"))
})

test_that("Formatting works for nace codes with letters", {
  x <- c("A", "01", "011", "0110", "01110")
  suppressWarnings(codes <- klassR:::formattering(x, classification = 6))
  expect_equal(codes, c("A", "01", "01.1", "01.10", "01.110"))
})

test_that("Formatting works for kommune", {
  x <- c("301", "0301", "1101", "1103")
  suppressWarnings(codes <- klassR:::formattering(x, classification = 131))
  expect_equal(codes, c("0301", "0301", "1101", "1103"))
})

test_that("Kommune fails when letters present", {
  x <- c("Oslo", "0301", "1101", "1103")
  expect_error(codes <- klassR:::formattering(x, classification = 131))
})
