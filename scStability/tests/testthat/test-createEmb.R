test_that("All embeddings are generated correctly", {
  input_pca <- matrix(stats::rnorm(200, mean = 20, sd = 2), ncol = 10)
  result <- scStability::createEmb(input_pca, n_runs = 10, n_cores = 8)
  expect_equal(length(result), 10)
  expect_equal(dim(result[[1]]), c(20, 2))
})
