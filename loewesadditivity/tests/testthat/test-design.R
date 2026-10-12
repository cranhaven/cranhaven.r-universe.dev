test_that("design_experiment works", {
 out <- design_experiment(n_rep = 2)
  expect_true(is.null(out))
})


