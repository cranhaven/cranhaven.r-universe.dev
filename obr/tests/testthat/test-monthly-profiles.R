# Monthly profiles tests. Unit tests are offline; parsing tests hit the
# live workbook and are skipped on CRAN.

v04_long_cols <- c("period", "period_type", "series",
                   "metric_type", "value", "unit")

test_that("month_to_period() maps the fiscal year correctly", {
  expect_equal(month_to_period("Apr", 2026), "2026-04")
  expect_equal(month_to_period("Dec", 2026), "2026-12")
  expect_equal(month_to_period("Jan", 2026), "2027-01")
  expect_equal(month_to_period("Mar", 2026), "2027-03")
  # Vectorised
  expect_equal(month_to_period(c("Apr", "Jan"), 2026),
               c("2026-04", "2027-01"))
})

test_that("get_monthly_profiles() validates the sheet argument", {
  expect_error(get_monthly_profiles("nonsense"), regexp = "arg")
})

test_that("get_monthly_profiles() returns tidy monthly profiles", {
  skip_on_cran()
  skip_if_offline()
  op <- options(obr.cache_dir = tempdir())
  on.exit(options(op), add = TRUE)

  mp <- get_monthly_profiles()
  expect_s3_class(mp, "obr_tbl")
  expect_true(all(v04_long_cols %in% names(mp)))
  expect_true(all(mp$period_type %in% c("month", "fiscal_year")))
  expect_true(all(mp$unit == "gbp_bn"))

  monthly <- mp[mp$period_type == "month", ]
  expect_match(monthly$period, "^[0-9]{4}-[0-9]{2}$", all = TRUE)
  expect_true("HMRC cash receipts" %in% monthly$series)

  # The 12 monthly values sum to the full-year forecast (small tolerance
  # for rounding in the published workbook).
  s <- "HMRC cash receipts"
  m_sum <- sum(monthly$value[monthly$series == s])
  fy    <- mp$value[mp$series == s & mp$period_type == "fiscal_year"]
  expect_equal(length(fy), 1L)
  expect_lt(abs(m_sum - fy), 1.0)

  prov <- obr_provenance(mp)
  expect_equal(prov$publication, "EFO-MP")
  expect_match(prov$source_url, "monthly-profiles")
})

test_that("get_monthly_profiles('cgncr') returns the CGNCR breakdown", {
  skip_on_cran()
  skip_if_offline()
  op <- options(obr.cache_dir = tempdir())
  on.exit(options(op), add = TRUE)

  cg <- get_monthly_profiles("cgncr")
  expect_s3_class(cg, "obr_tbl")
  expect_true("CGNCR" %in% cg$series)
  expect_true(any(cg$period_type == "month"))
})
