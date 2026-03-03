test_that("bundesrat endpoints return tibbles", {
  skip_if_offline()
  skip_on_cran()
  skip_on_ci()

  startlist <- bundesrat_startlist()
  expect_s3_class(startlist, "tbl_df")
  if (nrow(startlist) > 0) {
    expect_true(all(c("name", "url") %in% names(startlist)))
  }

  aktuelles <- bundesrat_aktuelles()
  expect_s3_class(aktuelles, "tbl_df")

  termine <- bundesrat_termine()
  expect_s3_class(termine, "tbl_df")

  plenum <- bundesrat_plenum_naechste_sitzungen()
  expect_s3_class(plenum, "tbl_df")

  stimmverteilung <- bundesrat_stimmverteilung()
  expect_s3_class(stimmverteilung, "tbl_df")
})
