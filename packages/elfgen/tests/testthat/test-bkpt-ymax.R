context("bkpt-ymax")
library(elfgen)

test.watershed.df <- data.frame(
  MAF = c(100, 200, 300, 400, 526, 600, 700, 800, 900, 1000),
  NT.TOTAL.UNIQUE = c(10, 20, 30, 40, 50, 40, 30 , 20, 10, 10),
  watershed.code = "testcode"
)


test_that("Function returns a dataframe", {
  expect_equal(bkpt_ymax(test.watershed.df),526)
})



