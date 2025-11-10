test_that("Notes are added to instances of prcomp.", {
  set.seed(10)
  test_pca <- prcomp(onze_intercepts |> select(-speaker), scale=T)
  rotated_pca <- pca_rotate_2d(test_pca, 10, pcs = c(3,6))
  expect_equal(
    rotated_pca[['note']],
    list("PC3 and PC6 rotated by 10 degrees (clockwise).")
  )
  rotated_pca <- pca_rotate_2d(rotated_pca, -15, pcs = c(1,2))
  expect_equal(
    rotated_pca[['note']],
    list(
      "PC3 and PC6 rotated by 10 degrees (clockwise).",
      "PC1 and PC2 rotated by -15 degrees (clockwise)."
    )
  )
})
