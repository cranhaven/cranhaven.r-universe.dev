test_that("quietly_run suppresses warnings", {

  warning_func_arugment <- function(info) {
    warning(info)
    return("Complete")
  }

  expect_warning(warning_func_arugment("Just checking"))

  result <- quietly_run(warning_func_arugment, "Just checking")

  expect_silent(result)

})

test_that("quietly_run suppresses errors", {

  message_func_no_argument <- function() {
    warning("Error!")
  }

  expect_warning(message_func_no_argument())

  result <- quietly_run(message_func_no_argument)

  expect_silent(result)

})
