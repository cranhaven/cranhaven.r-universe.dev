test_that("adjust_p return the correct class", {
  withr::local_package("Hmisc")
  adj <- summary(Species ~ .,
    data = iris,
    method = "reverse",
    test = TRUE
  ) %>%
    tidy_summary(prtest = "P") %>%
    adjust_p()

  expect_s3_class(adj, "tidy_summary")
})

test_that("warning and return if test is not set", {
  withr::local_package("Hmisc")
  no_test_adj <- summary(Species ~ .,
    data = iris,
    method = "reverse"
  ) %>%
    tidy_summary() %>%
    adjust_p()

  expect_s3_class(no_test_adj, "tidy_summary")
})
