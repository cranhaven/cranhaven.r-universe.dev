context("bkpt-pwit")
library(elfgen)

test.watershed.df <- data.frame(
  MAF = c(100, 200, 300, 400, 526, 600, 700, 800, 900, 1000),
  NT.TOTAL.UNIQUE = c(10, 20, 30, 40, 50, 40, 30 , 20, 10, 10),
  watershed.code = "testcode"
)


test_that("Function returns a dataframe", {
  expect_equal(bkpt_pwit(test.watershed.df, 0.80, 50, 1000), 526)
  expect_equal(bkpt_pwit(test.watershed.df, 0.60, 50, 1000), 400)
  expect_equal(bkpt_pwit(test.watershed.df, 0.80, "bhi"= 1000), 526)
})

test_that("Checking for No breakpoint identified", {
  expect_error(bkpt_pwit("watershed.df" = test.watershed.df, 0.80, 600, 1000),
               paste("No breakpoint identified using this set of inputs for ", test.watershed.df$watershed[1],
                     "\n  ... Try using a smaller quantile or a wider bounding range",
                     "\n  ... If still unsuccessful, a larger dataset may be required",sep=''))
})

test_that("Checking for quantile input parameter", {
  expect_error(bkpt_pwit("watershed.df" = test.watershed.df,
                         "blo" = 50,
                         "bhi"= 1000), "Missing quantile parameter")
})

