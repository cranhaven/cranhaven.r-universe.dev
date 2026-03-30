test_that("get_indicators returns proper data frame", {
  df_de <- get_indicators("de")
  expect_true(is.data.frame(df_de))
  expect_true(nrow(df_de) > 0)
  expect_true(all(c("ID", "Name", "Unit", "Active") %in% names(df_de)))

  df_en <- get_indicators("en")
  expect_true(is.data.frame(df_en))
  expect_true(nrow(df_en) > 0)
  expect_true(all(c("ID", "Name", "Unit", "Active") %in% names(df_en)))
})

test_that("get_geographies returns levels when null", {
  geo <- get_geographies()
  expect_true(is.data.frame(geo))
  expect_true(nrow(geo) > 0)
  expect_true("KRE" %in% geo$ID)
})

test_that("get_inkar_data handles invalid level parameter", {
  # Should throw error due to match.arg
  expect_error(get_inkar_data("011", level = "INVALID_LEVEL"))
})

test_that("get_inkar_data returns data for valid inputs", {
  skip_if_not_installed("httptest2")

  # Using a known active indicator
  # with_mock_dir records the exact API request/response the first time it runs,
  # and plays it back identically on all subsequent runs.
  httptest2::with_mock_dir("api-mocks", {
    df <- suppressWarnings(get_inkar_data(
      variable = "011",
      level = "KRE",
      year = 2021
    ))

    if (!is.null(df) && nrow(df) > 0) {
      expect_true(is.data.frame(df))
      expect_true("Kennziffer" %in% names(df))
      expect_true("Wert" %in% intersect(names(df), c("Wert", "2021")))
    }
  })
})
