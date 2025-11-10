test_that("Scaling works", {
  set.seed(10)
  onze_pca <- pca_test(onze_intercepts |> select(-speaker), n=10)
  onze_pca2 <- pca_test(onze_intercepts |> select(-speaker), scale = FALSE, n=10)
  scaled_eigen <- onze_pca$raw_data |>
    filter(source == "original") |>
    pull(eigenvalue) |>
    pluck(1)
  raw_eigen <- onze_pca2$raw_data |>
    filter(source == "original") |>
    pull(eigenvalue) |>
    pluck(1)
  expect_false(scaled_eigen == raw_eigen)
})

test_that("`pca_test()` generates all outputs", {
  onze_pca <- pca_test(onze_intercepts |> select(-speaker), n=10)
  expect_equal(
    names(onze_pca), c(
      "variance", "loadings", "raw_data", "variance_confint",
      "loadings_confint", "n"
    )
  )
})
