context("elfgen")
library(elfgen)


 test.watershed.df <- data.frame(
   MAF = c(100, 100, 200, 200, 300, 300, 400, 400, 526, 526, 600, 600, 700, 700, 800, 800, 900, 900, 1000, 1000),
   NT.TOTAL.UNIQUE = c(5, 10, 10, 20, 15, 30, 20, 40, 25, 50, 20, 40, 15, 30, 10, 20, 5, 10, 5, 10),
   watershed.code = "testcode"
 )


 test_that("Testing breakpt default when none is provided", {
    expect_equal(class(elfgen("watershed.df" = test.watershed.df,
                        "quantile" = 0.60,
                        "yaxis_thresh"= 53)), "list")
 })

 test_that("Testing yaxis_thresh default when none is provided", {
   expect_equal(class(elfgen("watershed.df" = test.watershed.df,
                             "quantile" = 0.60,
                             "breakpt" = 526)), "list")
 })

 test_that("Checking for successful upper.quant subset", {
    expect_error(elfgen(test.watershed.df, 0.50, 526, 53), "Unable to characterize upper subset using quantile regression") #0.50 quantile blows up rq() function with this dataset
    expect_error(elfgen(test.watershed.df, 0.70, 526, 53), "Upper subset contains fewer than 3 datapoints")
    expect_error(elfgen(test.watershed.df, 0.80, 526, 53), "Upper subset contains fewer than 3 datapoints")
  })

 test_that("Testing xlabel and ylabel overrides", {
   expect_equal(class(elfgen("watershed.df" = test.watershed.df,
                             "quantile" = 0.60,
                             "breakpt" = 526,
                             "yaxis_thresh"= 53,
                             "xlabel" = "Mean Annual Flow (ft3/s)",
                             "ylabel" = "Fish Species Richness")), "list")
 })

 test_that("Function returns a list of objects", {
    expect_equal(class(elfgen(test.watershed.df, 0.60, 526, 53)), "list")

 })

