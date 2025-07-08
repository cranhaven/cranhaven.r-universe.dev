skip_if_not_installed("torch")
skip_if_not(torch::torch_is_installed())

test_that("Output is a list with 23 elements", {
  expect_list(div(Z = as.matrix(rnorm(100), ncol = 1),
                       X = as.matrix(rnorm(100, sd = 2), ncol = 1),
                       Y = as.matrix(rnorm(100, mean = -3), ncol = 1),
                       num_epochs = 20, silent = TRUE), len = 23)
})

test_that("Output is a list with 23 elements, all inputs d-dim (d = 2 here)", {
  expect_list(div(Z = as.matrix(rnorm(100), ncol = 2),
                       X = as.matrix(rnorm(100, sd = 2), ncol = 2),
                       Y = as.matrix(rnorm(100, mean = -3), ncol = 2),
                       num_epochs = 20, silent = TRUE), len = 23)
})

test_that("Expected class is 'DIV'", {
  expect_class(div(Z = as.matrix(rnorm(100), ncol = 2),
                        X = as.matrix(rnorm(100, sd = 2), ncol = 2),
                        Y = as.matrix(rnorm(100, mean = -3), ncol = 2),
                        num_epochs = 20, silent = TRUE), "DIV")
})

test_that("DIV_f(x) returns a matrix -> needed for predict()", {
  expect_matrix(div(Z = as.matrix(rnorm(100), ncol = 2),
                      X = as.matrix(rnorm(100, sd = 2), ncol = 2),
                      Y = as.matrix(rnorm(100, mean = -3), ncol = 2),
                      num_epochs = 20, silent = TRUE)$DIV_f(x = torch_tensor(as.matrix(rnorm(50), ncol = 1), device = use_device())))
})

test_that("Sample sizes not equal", {
  expect_error(div(Z = as.matrix(rnorm(200), ncol = 2),
                        X = as.matrix(rnorm(100, sd = 2), ncol = 2),
                        Y = as.matrix(rnorm(100, mean = -3), ncol = 2),
                        num_epochs = 20, silent = TRUE))
})
