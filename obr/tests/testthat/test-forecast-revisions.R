test_that("get_forecast_revisions() errors on invalid unit", {
  expect_error(get_forecast_revisions(unit = "kgs"),
               regexp = "should be one of")
})

test_that("get_forecast_revisions() returns long-format obr_tbl", {
  skip_on_cran()
  skip_if_offline()

  res <- get_forecast_revisions(unit = "gbp_bn")
  expect_s3_class(res, "obr_tbl")
  expect_named(res, c("forecast_date", "component", "fiscal_year", "value"))
  expect_type(res$value, "double")

  # Top-level components present
  components <- unique(res$component)
  expect_true("Total"      %in% components)
  expect_true("Policy"     %in% components)
  expect_true("Underlying" %in% components)
  expect_true(any(grepl("Classification", components)))

  # Forecast dates look right
  expect_true(all(grepl("^[A-Z][a-z]+ [0-9]{4}$", res$forecast_date)))
  # Fiscal years look right
  expect_true(all(grepl("^[0-9]{4}-[0-9]{2}$", res$fiscal_year)))

  prov <- obr_provenance(res)
  expect_equal(prov$publication, "FRD")
  expect_match(prov$source_url, "forecast-revisions-database")
})

test_that("get_forecast_revisions(unit='pct_gdp') returns sensible magnitudes", {
  skip_on_cran()
  skip_if_offline()

  res <- get_forecast_revisions(unit = "pct_gdp")
  expect_s3_class(res, "obr_tbl")
  # PSNB revisions across vintages can be large in extreme periods
  # (e.g. COVID reclassifications), but should stay well within +/- 50% of GDP.
  expect_true(all(abs(res$value) < 50))
  # Median revision for any single vintage-year-component is small
  expect_lt(stats::median(abs(res$value)), 5)
})
