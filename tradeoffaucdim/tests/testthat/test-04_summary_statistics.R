
test_that("output", {
  expect_no_error(summary_stats(obj3))
  expect_type(summary_stats(obj3), type = "list")
  expect_type(summary_stats(obj3)$stepwise_process , type = "list")
  expect_type(summary_stats(obj3)$ordered_indep_vars , type = "character")
  expect_type(summary_stats(obj3)$bootstrap_data , type = "list")
  expect_type(summary_stats(obj3)$summary_stats , type = "list")
  expect_type(summary_stats(obj3)$models , type = "character")

})

