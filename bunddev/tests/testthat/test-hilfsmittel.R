test_that("hilfsmittel endpoints return tibbles", {
  skip_if_offline()
  skip_on_cran()
  skip_on_ci()

  tree <- hilfsmittel_tree(level = 1)
  expect_s3_class(tree, "tbl_df")

  if (nrow(tree) > 0 && "id" %in% names(tree)) {
    gruppe <- hilfsmittel_produktgruppe(tree$id[[1]])
    expect_s3_class(gruppe, "tbl_df")
  }
})
