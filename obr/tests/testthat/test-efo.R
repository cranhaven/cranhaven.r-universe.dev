test_that("list_efo_economy_measures() works without network", {
  result <- list_efo_economy_measures()

  expect_s3_class(result, "data.frame")
  expect_named(result, c("measure", "sheet", "description"))
  expect_equal(nrow(result), 3)
  expect_true("inflation"  %in% result$measure)
  expect_true("labour"     %in% result$measure)
  expect_true("output_gap" %in% result$measure)
})

test_that("get_efo_economy() errors on invalid measure", {
  expect_error(
    get_efo_economy("NOT_A_MEASURE"),
    regexp = "should be one of"
  )
})

# v0.4.0 schema: period, period_type, series, metric_type, value, unit.
v04_long_cols <- c("period", "period_type", "series",
                   "metric_type", "value", "unit")

test_that("get_efo_fiscal() returns the v0.4.0 long schema", {
  skip_on_cran()
  skip_if_offline()

  result <- get_efo_fiscal()

  expect_s3_class(result, "data.frame")
  expect_named(result, v04_long_cols)
  expect_type(result$period,      "character")
  expect_type(result$period_type, "character")
  expect_type(result$series,      "character")
  expect_type(result$metric_type, "character")
  expect_type(result$value,       "double")
  expect_type(result$unit,        "character")
  expect_gt(nrow(result), 10)
  expect_true("Net borrowing"    %in% result$series)
  expect_true("Current receipts" %in% result$series)
  expect_true(all(result$period_type == "fiscal_year"))
  expect_true(all(result$metric_type == "level"))
  expect_true(all(result$unit == "gbp_bn"))
})

test_that("get_efo_fiscal() covers 5-year forecast horizon", {
  skip_on_cran()
  skip_if_offline()

  result <- get_efo_fiscal()
  years  <- unique(result$period)

  expect_gte(length(years), 5)
  expect_true(all(grepl("^[0-9]{4}-[0-9]{2}$", years)))
})

test_that("get_efo_economy('inflation') returns the v0.4.0 schema", {
  skip_on_cran()
  skip_if_offline()

  result <- get_efo_economy("inflation")

  expect_s3_class(result, "data.frame")
  expect_named(result, v04_long_cols)
  expect_gt(nrow(result), 200)
  expect_true(any(grepl("CPI",  result$series)))
  expect_true(any(grepl("CPIH", result$series)))
  expect_true(all(grepl("^[0-9]{4}Q[1-4]$", result$period)))
  expect_true(all(result$period_type == "quarter"))
})

test_that("get_efo_economy('inflation') splits Index vs YoY into metric_type", {
  # This is the v0.4.0 fix for OBR feedback (Ben Northcott, 2026-04-29):
  # previously, CPI Index values (~135) and CPI YoY values (~2.1) sat in the
  # same `value` column with no machine-readable distinction. Now they are
  # tagged via metric_type ("index" vs "yoy_pct") and unit ("index" vs "pct").
  skip_on_cran()
  skip_if_offline()

  result <- get_efo_economy("inflation")
  types  <- unique(result$metric_type)

  expect_true(any(types %in% c("index", "yoy_pct", "pct", "level")))
  # Index rows and YoY rows should each be filterable independently
  if ("index" %in% types) {
    idx <- result[result$metric_type == "index", ]
    expect_gt(nrow(idx), 0)
    expect_true(all(idx$unit == "index"))
  }
  if ("yoy_pct" %in% types) {
    yoy <- result[result$metric_type == "yoy_pct", ]
    expect_gt(nrow(yoy), 0)
    expect_true(all(yoy$unit == "pct"))
  }
})

test_that("get_efo_economy('labour') returns the v0.4.0 schema", {
  skip_on_cran()
  skip_if_offline()

  result <- get_efo_economy("labour")

  expect_s3_class(result, "data.frame")
  expect_named(result, v04_long_cols)
  expect_gt(nrow(result), 200)
  expect_true(any(grepl("Employment", result$series)))
  expect_true(any(grepl("unemployment", result$series, ignore.case = TRUE)))
  expect_true(all(result$period_type == "quarter"))
})

test_that("get_efo_economy('output_gap') returns the v0.4.0 schema", {
  skip_on_cran()
  skip_if_offline()

  result <- get_efo_economy("output_gap")

  expect_s3_class(result, "data.frame")
  expect_named(result, v04_long_cols)
  expect_gt(nrow(result), 100)
  expect_true(all(result$series == "Output gap"))
  expect_true(any(grepl("^197", result$period)))
  expect_true(all(result$period_type == "quarter"))
  expect_true(all(result$metric_type == "pct"))
  expect_true(all(result$unit == "pct"))
})

test_that("EFO functions return obr_tbl with EFO provenance", {
  skip_on_cran()
  skip_if_offline()

  for (call in list(quote(get_efo_fiscal()),
                    quote(get_efo_economy("inflation")),
                    quote(get_efo_economy("output_gap")))) {
    res  <- eval(call)
    expect_s3_class(res, "obr_tbl")
    prov <- obr_provenance(res)
    expect_equal(prov$publication, "EFO")
    expect_match(prov$source_url, "economic-and-fiscal-outlook")
    expect_match(prov$vintage, "^[A-Z][a-z]+ [0-9]{4}$")
  }
})
