test_that("forecast_date_to_index() orders OBR-style dates correctly", {
  d <- c("November 2010", "March 2011", "July 2015",
         "December 2014", "March 2024")
  idx <- forecast_date_to_index(d)
  expect_type(idx, "integer")
  expect_equal(length(idx), length(d))
  # Ordering matches calendar order
  ordered <- d[order(idx)]
  expect_equal(ordered,
               c("November 2010", "March 2011", "December 2014",
                 "July 2015", "March 2024"))
})

test_that("forecast_date_to_index() returns NA on un-recognised input", {
  expect_true(is.na(forecast_date_to_index("not a date")))
  expect_true(is.na(forecast_date_to_index("Foo 2024")))
})

test_that("obr_forecast_panel() returns a wide obr_tbl with provenance", {
  skip_on_cran()
  skip_if_offline()

  panel <- obr_forecast_panel("PSNB")
  expect_s3_class(panel, "obr_tbl")
  expect_true("forecast_date" %in% names(panel))
  # At least one fiscal year column
  yr_cols <- setdiff(names(panel), "forecast_date")
  expect_gt(length(yr_cols), 5L)
  expect_true(all(grepl("^[0-9]{4}-[0-9]{2}$", yr_cols)))
  # Numeric values
  expect_true(is.numeric(panel[[yr_cols[1L]]]))

  prov <- obr_provenance(panel)
  expect_equal(prov$publication, "HFD")
  expect_match(prov$notes, "real-time panel")
})
