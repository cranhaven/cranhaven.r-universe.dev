# Define a custom data frame for testing
custom_df <- data.frame(
  cor = c(0.5, -0.3, 0.8, -0.2, 0.9),
  sample = c(1, 2, 3, 4, 5)
)

test_that("cor_diff correctly determines orientation when more positive correlations", {
  result <- cor_diff(custom_df)
  expect_equal(result, c(2, 4))
})

test_that("cor_diff correctly determines orientation when more negative correlations", {
  custom_df$cor <- c(-0.5, -0.3, 0.8, -0.2, -0.9)
  result <- cor_diff(custom_df)
  expect_equal(result, c(3))
})
