

test_that("output", {


  expect_no_error(compare_test(obj5))
  expect_type(compare_test(obj5), type = "list")


  obj_v2 <- suppressMessages({apply_model(obj2,
                                          test_partition_prop = 0.5,
                                          models = c("SL.glm", "SL.speedglm",
                                                     "SL.rpart"))}) %>%
    summary_stats() %>%
    plot_curve()

  expect_no_error(suppressWarnings(compare_test(obj_v2)))
  expect_type(suppressWarnings(compare_test(obj_v2)), type = "list")

  obj_v3 <- suppressMessages({apply_model(obj2,
                                          test_partition_prop = 0.5,
                                          models = c("SL.glm"))}) %>%
    summary_stats() %>%
    plot_curve()

  expect_no_error(compare_test(obj_v3))
  expect_type(compare_test(obj_v3), type = "list")

})


