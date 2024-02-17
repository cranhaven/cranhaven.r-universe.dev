context("richness-change")
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
    expect_equal(richness_change(test.watershed.df, "pctchg" = 10),-0.2465436)
 })

