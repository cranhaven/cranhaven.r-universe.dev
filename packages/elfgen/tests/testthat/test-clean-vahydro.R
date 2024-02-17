context("clean-vahydro")
library(elfgen)

test.watershed.df <- data.frame(
  MAF = c(100, 200, 300, 400, 526, 600, 700, 800, 400, 900, 1000, 100, 100),
  NT.TOTAL.UNIQUE = c(10, 20, 30, 40, 50, 40, 30 , 20, 50, 10, 10,99999,87),
  watershed.code = "test_testcode",
  hydrocode = c("t1","t2","t3","t4","t5","t6","t7","t8","t9","t10","t11","t12","t13"),
  DA_SQMI = c(110, 220000, 280, 360, 530, 604, 712, 698, 40000, 905, 1087, 98, 87),
  x.metric = c(100, 200, 300, 400, 526, 600, 700, 800, 400, 900, 1000, 100, 100)
)

test_that("Function removes 4 rows and returns a cleaned dataframe", {
  expect_equal(length(test.watershed.df[,1])-length(clean_vahydro(test.watershed.df)[,1]), 4) #test that data is cleaned - 4 stations are removed
})
