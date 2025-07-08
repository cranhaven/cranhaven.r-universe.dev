skip_if_not_installed("torch")
skip_if_not(torch::torch_is_installed())

test_that("Energy loss has length 1 and positive", {
  expect_numeric(as.numeric(energyloss(x0 = torch_randn(5, 2),
                            x = torch_randn(5, 2),
                            xp = torch_randn(5, 2),
                            verbose = FALSE)), lower = 0, len = 1L)
})

test_that("Works correctly with tensors", {
  expect_class(energyloss(x0 = torch_randn(5, 2),
                          x = torch_randn(5, 2),
                          xp = torch_randn(5, 2),
                          verbose = FALSE), "torch_tensor")
})
