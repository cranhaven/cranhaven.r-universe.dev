skip_if_not_installed("torch")
skip_if_not(torch::torch_is_installed())

test_that("Output is a list with three elements", {
  expect_list(divfit(Z = as.matrix(rnorm(100), ncol = 2),
                     X = as.matrix(rnorm(100, sd = 2), ncol = 2),
                     Y = as.matrix(rnorm(100, mean = -3), ncol = 2),
                     W = NULL,
                     epsx_dim = 50, epsy_dim = 50, epsh_dim = 50,
                     hidden_dim = 100, num_layer = 3,
                     num_epochs = 20, lr = 10^(-3), beta = 1, silent = TRUE), len = 3)
})

test_that("Output is a list with three elements, all inputs d-dim (d = 2 here)", {
  expect_list(divfit(Z = as.matrix(rnorm(100), ncol = 2),
                     X = as.matrix(rnorm(100, sd = 2), ncol = 2),
                     Y = as.matrix(rnorm(100, mean = -3), ncol = 2),
                     W = NULL,
                     epsx_dim = 50, epsy_dim = 50, epsh_dim = 50,
                     hidden_dim = 100, num_layer = 3,
                     num_epochs = 20, lr = 10^(-3), beta = 1, silent = TRUE), len = 3)
})

test_that("Matrix of loss values returned", {
  expect_matrix(divfit(Z = as.matrix(rnorm(100), ncol = 2),
                       X = as.matrix(rnorm(100, sd = 2), ncol = 2),
                       Y = as.matrix(rnorm(100, mean = -3), ncol = 2),
                       W = NULL,
                       epsx_dim = 50, epsy_dim = 50, epsh_dim = 50,
                       hidden_dim = 100, num_layer = 3,
                       num_epochs = 20, lr = 10^(-3), beta = 1, silent = TRUE)$loss_vec,
                ncols = 3, nrows = 20)
})

test_that("Sample sizes not equal", {
  expect_error(divfit(Z = as.matrix(rnorm(100), ncol = 2),
                      X = as.matrix(rnorm(100, sd = 2), ncol = 2),
                      Y = as.matrix(rnorm(200, mean = -3), ncol = 2),
                      W = NULL,
                      epsx_dim = 50, epsy_dim = 50, epsh_dim = 50,
                      hidden_dim = 100, num_layer = 3,
                      num_epochs = 20, lr = 10^(-3), beta = 1, silent = TRUE))
})
