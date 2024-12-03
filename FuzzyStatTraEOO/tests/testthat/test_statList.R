
test_that("StatList instance works as expected (initialize)", {
  ## CONSTRUCTOR ERRORS
  ## more parameters than needed
  expect_error(StatList$new(3))
  expect_error(StatList$new("a"))
  expect_error(StatList$new(3L))
  expect_error(StatList$new(3.0))
  expect_error(StatList$new(list()))

  ## works as expected
  expect_error(StatList$new())
  expect_output(expect_error(StatList$new()), "this an abstract class and it can't be initialized.")

})
