context("elfchange")
library(elfgen)


test.watershed.df <- data.frame(
  watershed = "02080201",
  m = 2.34,
  b = 9.19,
  rsquared = 0.806,
  rsquared_adj = 0.8,
  p = 0,
  n_total = 861,
  n_subset = 705,
  n_subset_upper = 35
)

 test_that("Function returns a ggplot object", {
    expect_equal(class(elfchange(test.watershed.df,
                                 "yaxis_thresh" = 25,
                                 "xlabel" = "test x label",
                                 "ylabel" = "test y label")),
                 c("gg", "ggplot"))

 })
 #
 # test_that("Function returns a ggplot object, uses default yaxis_thresh, xlabel, ylabel", {
 #   expect_equal(class(elfchange(test.watershed.df)),c("gg", "ggplot"))
 #
 # })
