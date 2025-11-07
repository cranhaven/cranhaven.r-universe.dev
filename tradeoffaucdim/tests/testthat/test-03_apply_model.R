

test_that("output", {
  obj <- suppressMessages({apply_model(obj2,test_partition_prop =  0.5)})
  expect_no_error(obj)
  expect_type(obj, type = "list")
  expect_type(obj$stepwise_process , type = "list")
  expect_type(obj$ordered_indep_vars , type = "character")
  expect_type(obj$bootstrap_data , type = "list")

})
