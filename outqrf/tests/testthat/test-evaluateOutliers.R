X <- data.frame(a = 1:100, b = 1:100)
X_outlier <- data.frame(a = 1:100, b = 1:100)
X_outlier[1L, "a"] <- 1000

test_that("Generate results as expected with iris", {
    anomaly_data <- generateOutliers(iris, p = 0.05, sd_factor = 5, seed = 123)
    qrf<- outqrf(anomaly_data,verbose = F)
    result<- evaluateOutliers(iris,anomaly_data,qrf$outliers)
    expect_type(result, "double")
    expect_length(result, n = 5)
    expect_true(length(result) > 0)
})

test_that("Generate results as expected with 2 cols data", {
    qrf<- outqrf(X_outlier,verbose = F)
    result<- evaluateOutliers(X,X_outlier,qrf$outliers)
    expect_type(result, "double")
    expect_length(result, n = 5)
    expect_true(length(result) > 0)
})
