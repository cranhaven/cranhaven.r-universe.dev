X <- data.frame(a = 1:100, b = 1:100)
X[1L, "a"] <- 1000
X_NA <- X
X_NA[2L, "a"] <- NA

test_that("outqrf(iris) Generate results as expected", {
    qrf <- outqrf(iris)
    expect_true(nrow(qrf$outliers)>0)
})

test_that("outqrf(iris,quantiles_type=400) Generate results as expected", {
    qrf <- outqrf(iris)
    expect_true(nrow(qrf$outliers)>0)
})

test_that("verbose=0 has an effect", {
    expect_silent(qrf<- outqrf(iris,quantiles_type=400,verbose=0))
})

test_that("imputation on data with 2 cols can work", {
    qrf <- outqrf(X_NA,quantiles_type=400,impute=TRUE)
    expect_true(nrow(qrf$outliers)>0)
})

test_that("imputation on iris data with missing can work", {
    nrow <- nrow(iris)
    ncol <- ncol(iris)
    iris_NA<- iris
    num_missing <- 20
    missing_rows <- sample(1:nrow, num_missing)
    missing_cols <- sample(1:ncol, ncol)
    iris_NA[missing_rows, missing_cols] <- NA
    qrf <- outqrf(iris_NA,quantiles_type=400,impute=TRUE)
    expect_true(nrow(qrf$outliers)>0)
})

test_that("threshold can work normally in marginal scenarios.", {
    qrf <- outqrf(iris,threshold=0)
    expect_true(nrow(qrf$outliers)==0)
})

test_that("threshold can work normally in marginal scenarios.", {
    sum_numeric<-sum(sapply(iris, is.numeric))
    qrf <- outqrf(iris,threshold=1)
    expect_true(nrow(qrf$outliers)==sum_numeric*nrow(iris))
})

