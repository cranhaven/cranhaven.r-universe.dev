skip_if_not_installed("torch")
skip_if_not(torch::torch_is_installed())

test_that("Returns nothing, just prints", {
  expect_null(print.DIV(div(Z = as.matrix(rnorm(100), ncol = 1),
                    X = as.matrix(rnorm(100, sd = 2), ncol = 1),
                    Y = as.matrix(rnorm(100, mean = -3), ncol = 1),
                    num_epochs = 20, silent = TRUE)))
})
