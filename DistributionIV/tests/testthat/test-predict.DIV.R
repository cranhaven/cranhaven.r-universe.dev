skip_if_not_installed("torch")
skip_if_not(torch::torch_is_installed())

div_modelW <- div(Z = as.matrix(rnorm(100)),
                  X = as.matrix(rchisq(100, df = 3)),
                  Y = as.matrix(rnorm(100, mean = -2)),
                  W = as.matrix(rnorm(100, mean = 3)),
                  num_epochs = 20, silent = TRUE)

div_modelnoW <- div(Z = as.matrix(rnorm(100)),
                    X = as.matrix(rchisq(100, df = 3)),
                    Y = as.matrix(rnorm(100, mean = -2)),
                    num_epochs = 20, silent = TRUE)

test_that("Error if Xtest contains another # vars than Xtrain", {
  expect_error(predict(div_modelnoW, Xtest = matrix(rnorm(40, mean = 2), ncol = 2)))
})

test_that("Vector of mean values of proper length returned", {
  expect_vector(predict(div_modelnoW, Xtest = matrix(rnorm(40, mean = 2), ncol = 1)),
                size = 40)
})

test_that("Matrix of sampled values of proper dimension returned", {
  expect_matrix(predict(div_modelnoW, Xtest = matrix(rnorm(40, mean = 2), ncol = 1),
                        type = "sample", nsample = 200),
                nrows = 40, ncols = 200)
})

test_that("Matrix of quantile values of proper dimension returned", {
  expect_matrix(predict(div_modelnoW, Xtest = matrix(rnorm(40, mean = 2), ncol = 1),
                        type = "quantile", nsample = 200),
                nrows = 40, ncols = 9)
})

test_that("Vector of quantile values of proper length returned", {
  expect_vector(predict(div_modelnoW, Xtest = matrix(rnorm(40, mean = 2), ncol = 1),
                        type = "quantile", nsample = 200, quantiles = 0.05),
                size = 40)
})

test_that("Warning about conditional effects", {
  expect_error(predict.DIV(div_modelW, Xtest = rnorm(10)))
})

test_that("Error because of different lengths", {
  expect_error(predict.DIV(div_modelW, Xtest = rnorm(10), Wtest = rnorm(20)))
})

