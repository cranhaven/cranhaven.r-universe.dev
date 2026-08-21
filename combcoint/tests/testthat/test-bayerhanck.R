# load data
data(lutkepohl_e1)
# run Bayerhanck
result <- bayerhanck(linvestment ~ lincome + lconsumption,
                     data = lutkepohl_e1, lags = 4)

test_that("combined bayerhanck test statistics", {
  expect_equal(
    round(result$bh.test, 4)
    , 9.1603)
})

test_that("combined bayerhanck p-value", {
  expect_equal(
    round(as.numeric(result$bh.pval), 4)
    , 0.3111)
})

test_that("p-values of the underlying tests", {
  expect_equal(
    round(as.numeric(result$pval.stat), 4)
    , c(0.2484, 0.3532, 0.3975, 0.2940)
    )
})
