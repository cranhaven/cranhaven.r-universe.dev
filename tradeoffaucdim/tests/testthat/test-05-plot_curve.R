

test_that("output", {
  expect_no_error(plot_curve(obj4))
  expect_type(plot_curve(obj4), type = "list")
  expect_type(plot_curve(obj4)$stepwise_process , type = "list")
  expect_type(plot_curve(obj4)$ordered_indep_vars , type = "character")
  expect_type(plot_curve(obj4)$bootstrap_data , type = "list")
  expect_type(plot_curve(obj4)$summary_stats , type = "list")
  expect_type(plot_curve(obj4)$models , type = "character")
  expect_type(plot_curve(obj4)$plot_performance , type = "list")
  expect_type(plot_curve(obj4)$plot_time , type = "list")
})


