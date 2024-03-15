test_that("gbins.quantiles works", {
    expect_regression(gbins.quantiles("test.fixedbin", c(0, 0.2, 0.3, 0.9, 1.2), "test.sparse", percentiles = c(0.2, 0.5, 0.6), iterator = 10), "gbins.quantiles.1")
    expect_regression(gbins.quantiles("test.fixedbin", c(0, 0.2, 0.3, 0.9, 1.2), "test.sparse", percentiles = c(0.2, 0.5, 0.6), iterator = 100), "gbins.quantiles.2")
})

test_that("gbins.summary works", {
    expect_regression(gbins.summary("test.fixedbin", c(0, 0.2, 0.3, 0.9, 1.2), "test.sparse", iterator = 100), "gbins.summary.1")
})
