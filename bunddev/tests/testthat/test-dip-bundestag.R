test_that("dip bundestag list endpoints return tibbles", {
  skip_if_offline()
  skip_on_cran()

  if (Sys.getenv("DIP_BUNDESTAG_API_KEY") == "") {
    Sys.setenv(DIP_BUNDESTAG_API_KEY = "OSOegLs.PR2lwJ1dwCeje9vTj7FPOt3hvpYKtwKkhw")
  }

  date_params <- list(f.datum.start = "2024-06-01", f.datum.end = "2024-06-30")
  expected_cols <- c("num_found", "cursor", "documents")

  results <- dip_bundestag_vorgang_list(params = date_params)
  expect_s3_class(results, "tbl_df")
  expect_true(all(expected_cols %in% names(results)))
  expect_gt(results$num_found, 0)

  results <- dip_bundestag_person_list(params = list())
  expect_s3_class(results, "tbl_df")
  expect_true(all(expected_cols %in% names(results)))
  expect_gt(results$num_found, 0)

  results <- dip_bundestag_drucksache_list(params = date_params)
  expect_s3_class(results, "tbl_df")
  expect_true(all(expected_cols %in% names(results)))
  expect_gt(results$num_found, 0)

  results <- dip_bundestag_drucksache_text_list(params = date_params)
  expect_s3_class(results, "tbl_df")
  expect_true(all(expected_cols %in% names(results)))

  results <- dip_bundestag_plenarprotokoll_list(params = date_params)
  expect_s3_class(results, "tbl_df")
  expect_true(all(expected_cols %in% names(results)))
  expect_gt(results$num_found, 0)

  results <- dip_bundestag_plenarprotokoll_text_list(params = date_params)
  expect_s3_class(results, "tbl_df")
  expect_true(all(expected_cols %in% names(results)))

  results <- dip_bundestag_aktivitaet_list(params = date_params)
  expect_s3_class(results, "tbl_df")
  expect_true(all(expected_cols %in% names(results)))
  expect_gt(results$num_found, 0)

  results <- dip_bundestag_vorgangsposition_list(params = date_params)
  expect_s3_class(results, "tbl_df")
  expect_true(all(expected_cols %in% names(results)))
  expect_gt(results$num_found, 0)
})

test_that("dip bundestag detail endpoints return tibbles", {
  skip_if_offline()
  skip_on_cran()

  if (Sys.getenv("DIP_BUNDESTAG_API_KEY") == "") {
    Sys.setenv(DIP_BUNDESTAG_API_KEY = "OSOegLs.PR2lwJ1dwCeje9vTj7FPOt3hvpYKtwKkhw")
  }

  result <- dip_bundestag_vorgang(302813)
  expect_s3_class(result, "tbl_df")
  expect_true("id" %in% names(result))

  result <- dip_bundestag_person(1728)
  expect_s3_class(result, "tbl_df")
  expect_true(all(c("id", "nachname", "vorname") %in% names(result)))

  result <- dip_bundestag_drucksache(275132)
  expect_s3_class(result, "tbl_df")
  expect_true(all(c("id", "drucksachetyp") %in% names(result)))

  result <- dip_bundestag_drucksache_text(275132)
  expect_s3_class(result, "tbl_df")
  expect_true("id" %in% names(result))

  result <- dip_bundestag_plenarprotokoll(908)
  expect_s3_class(result, "tbl_df")
  expect_true(all(c("id", "dokumentnummer") %in% names(result)))

  result <- dip_bundestag_plenarprotokoll_text(908)
  expect_s3_class(result, "tbl_df")
  expect_true("id" %in% names(result))

  result <- dip_bundestag_aktivitaet(1493545)
  expect_s3_class(result, "tbl_df")
  expect_true(all(c("id", "aktivitaetsart") %in% names(result)))

  result <- dip_bundestag_vorgangsposition(654805)
  expect_s3_class(result, "tbl_df")
  expect_true(all(c("id", "vorgangsposition") %in% names(result)))
})

test_that("dip bundestag list endpoints return distinct data", {
  skip_if_offline()
  skip_on_cran()

  if (Sys.getenv("DIP_BUNDESTAG_API_KEY") == "") {
    Sys.setenv(DIP_BUNDESTAG_API_KEY = "OSOegLs.PR2lwJ1dwCeje9vTj7FPOt3hvpYKtwKkhw")
  }

  date_params <- list(f.datum.start = "2024-06-01", f.datum.end = "2024-06-30")

  vorgang <- dip_bundestag_vorgang_list(params = date_params)
  drucksache <- dip_bundestag_drucksache_list(params = date_params)
  plenarprotokoll <- dip_bundestag_plenarprotokoll_list(params = date_params)

  vorgang_docs <- vorgang$documents[[1]]
  drucksache_docs <- drucksache$documents[[1]]
  plenarprotokoll_docs <- plenarprotokoll$documents[[1]]

  # Different endpoint types should return different column structures
  expect_false(identical(names(vorgang_docs[[1]]), names(drucksache_docs[[1]])))
  expect_false(identical(names(vorgang_docs[[1]]), names(plenarprotokoll_docs[[1]])))
})
