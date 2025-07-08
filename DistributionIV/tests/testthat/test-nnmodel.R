skip_if_not_installed("torch")
skip_if_not(torch::torch_is_installed())

test_that("Is a neural network module.", {
  expect_class(nn_model(in_dim = 1, noise_dim = 100,
                        out_dim = 1), "nn_module")
})
