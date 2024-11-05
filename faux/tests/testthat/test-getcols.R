test_that("error", {
  expect_error(getcols())
  expect_null(getcols(mtcars))
  expect_error(getcols(mtcars, 100))
  expect_error(getcols(mtcars, "no"))
  expect_error(getcols(mtcars, no))
})


test_that("basic", {
  expect_equal(getcols(mtcars, 1), "mpg")
  expect_equal(getcols(mtcars, 1L), "mpg")
  expect_equal(getcols(mtcars, "mpg"), "mpg")
  expect_equal(getcols(mtcars, mpg), "mpg")
  expect_equal(getcols(mtcars, 1:2), c("mpg", "cyl"))
  
  m <- "mpg"
  expect_equal(getcols(mtcars, !!m), "mpg")
})