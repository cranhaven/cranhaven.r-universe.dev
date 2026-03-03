test_that("abfallnavi endpoints return tibbles", {
  skip_if_offline()
  skip_on_cran()

  orte <- abfallnavi_orte()
  expect_s3_class(orte, "tbl_df")

  if (nrow(orte) > 0) {
    ort_id <- orte$id[[1]]
    strassen <- abfallnavi_strassen(ort_id)
    expect_s3_class(strassen, "tbl_df")

    fraktionen <- abfallnavi_fraktionen()
    expect_s3_class(fraktionen, "tbl_df")

    if (nrow(strassen) > 0 && nrow(fraktionen) > 0) {
      termine <- abfallnavi_termine_strassen(strassen$id[[1]], fraktion = fraktionen$id[[1]])
      expect_s3_class(termine, "tbl_df")
    }
  }
})
