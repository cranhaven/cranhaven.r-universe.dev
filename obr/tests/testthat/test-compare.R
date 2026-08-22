test_that("obr_compare_vintages() errors on unknown 'what'", {
  expect_error(
    obr_compare_vintages("October 2024", "March 2026", what = "NOT_A_TABLE"),
    regexp = "Unknown"
  )
})

test_that("obr_actual_vs_forecast() errors on unsupported series", {
  expect_error(
    obr_actual_vs_forecast("CPI"),
    regexp = "should be one of"
  )
})

test_that("obr_compare_vintages('fiscal') returns the expected diff schema", {
  skip_on_cran()
  skip_if_offline()

  diff <- obr_compare_vintages("October 2024", "March 2026", what = "fiscal")

  expect_s3_class(diff, "obr_tbl")
  expect_named(
    diff,
    c("period", "period_type", "series", "metric_type", "unit",
      "value_a", "value_b", "revision")
  )
  expect_type(diff$value_a,  "double")
  expect_type(diff$value_b,  "double")
  expect_type(diff$revision, "double")
  # All rows are inner-joined: should be present in both vintages
  expect_true(all(!is.na(diff$value_a)))
  expect_true(all(!is.na(diff$value_b)))
  # revision must equal value_b - value_a
  expect_equal(diff$revision, diff$value_b - diff$value_a)
  # Should cover Net borrowing across at least one period
  expect_true("Net borrowing" %in% diff$series)
  expect_true(all(diff$period_type == "fiscal_year"))
  expect_true(all(diff$unit == "gbp_bn"))
})

test_that("obr_compare_vintages('inflation') returns metric_type-aware diff", {
  skip_on_cran()
  skip_if_offline()

  diff <- obr_compare_vintages("October 2024", "March 2026", what = "inflation")

  expect_s3_class(diff, "obr_tbl")
  expect_named(
    diff,
    c("period", "period_type", "series", "metric_type", "unit",
      "value_a", "value_b", "revision")
  )
  expect_true(all(diff$period_type == "quarter"))
  # CPI series should be present and tagged yoy_pct/pct
  cpi <- diff[diff$series == "CPI", ]
  expect_gt(nrow(cpi), 0)
  expect_true(all(cpi$metric_type == "yoy_pct"))
  expect_true(all(cpi$unit == "pct"))
})

test_that("obr_actual_vs_forecast('PSNB') joins HFD against PFD outturn", {
  skip_on_cran()
  skip_if_offline()

  eval <- obr_actual_vs_forecast("PSNB")

  expect_s3_class(eval, "obr_tbl")
  expect_named(
    eval,
    c("forecast_date", "period", "period_type", "series", "unit",
      "value_forecast", "value_actual", "error")
  )
  expect_type(eval$value_forecast, "double")
  expect_type(eval$value_actual,   "double")
  expect_type(eval$error,          "double")
  expect_equal(eval$error, eval$value_forecast - eval$value_actual)
  # All series are PSNB
  expect_true(all(eval$series == "PSNB"))
  expect_true(all(eval$unit == "gbp_bn"))
  expect_gt(nrow(eval), 50)
  # 2020-21 should have many forecasts paired against the (very large) outturn
  e_covid <- eval[eval$period == "2020-21", ]
  expect_gt(nrow(e_covid), 5)
})

test_that("obr_actual_vs_forecast() preserves HFD provenance", {
  skip_on_cran()
  skip_if_offline()

  eval <- obr_actual_vs_forecast("PSNB")
  prov <- obr_provenance(eval)
  expect_equal(prov$publication, "HFD")
  expect_match(prov$notes, "Outturn source")
})

test_that("obr_compare_vintages() invariant: revision == value_b - value_a", {
  # Level 3 invariant test. Holds by construction but worth pinning.
  skip_on_cran()
  skip_if_offline()

  diff <- obr_compare_vintages("October 2024", "March 2026", what = "fiscal")
  expect_equal(diff$revision, diff$value_b - diff$value_a, tolerance = 1e-10)
})

test_that("obr_compare_vintages() with same vintage returns zero revisions", {
  # Self-comparison invariant: if a == b, every revision must be exactly 0.
  skip_on_cran()
  skip_if_offline()

  diff <- obr_compare_vintages("March 2026", "March 2026", what = "fiscal")
  expect_gt(nrow(diff), 0)
  expect_true(all(diff$revision == 0))
  expect_equal(diff$value_a, diff$value_b)
})

test_that("obr_actual_vs_forecast() invariant: error == forecast - actual", {
  # Level 3 invariant. Pin it.
  skip_on_cran()
  skip_if_offline()

  eval <- obr_actual_vs_forecast("PSNB")
  expect_equal(eval$error,
               eval$value_forecast - eval$value_actual,
               tolerance = 1e-10)
})

test_that("obr_compare_vintages() accepts catalogue table ids and rejects junk", {
  expect_error(obr_compare_vintages("March 2025", "March 2026", what = "99.99"),
               regexp = "Unknown .*what")
  expect_error(obr_compare_vintages("March 2025", "March 2026", what = 6.5),
               regexp = "single character")
  # A catalogue id resolves to a fetcher without error (no network here)
  fn <- .compare_fn("6.13")
  expect_true(is.function(fn))
})
