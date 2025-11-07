library(testthat)

test_that("output", {
  expect_no_error(define_indepvars(obj1))
  expect_type(define_indepvars(obj1), type = "list")
  expect_type(define_indepvars(obj1)$stepwise_process , type = "list")
  expect_type(define_indepvars(obj1)$ordered_indep_vars , type = "character")
  expect_type(define_indepvars(obj1)$bootstrap_data , type = "list")

})
