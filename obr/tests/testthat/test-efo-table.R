v04_long_cols <- c("period", "period_type", "series",
                   "metric_type", "value", "unit")

# ----- Catalogue -----------------------------------------------------------

test_that("obr_efo_catalogue() returns a populated catalogue", {
  cat <- obr_efo_catalogue()
  expect_s3_class(cat, "data.frame")
  expect_named(cat, c("table_id", "file", "section", "title",
                      "layout", "default_metric_type", "default_unit"))
  # 17 aggregates + 22 economy = 39 rows
  expect_gte(nrow(cat), 39)
  expect_true("6.5" %in% cat$table_id)
  expect_true("1.7" %in% cat$table_id)
  expect_true("1.14" %in% cat$table_id)
  expect_true(all(cat$file %in% c("aggregates", "economy")))
  expect_true(all(cat$layout %in%
                    c("quarterly_wide", "quarterly_single",
                      "annual_year_wide", "annual_period_wide",
                      "fiscal_year_wide",
                      "subsector_matrix", "quarterly_indented",
                      "cross_reference")))
})

# ----- Argument validation -------------------------------------------------

test_that("get_efo_table() errors on unknown table id", {
  expect_error(get_efo_table("99.99"), regexp = "Unknown EFO table id")
})

test_that("get_efo_table() errors on non-string table id", {
  expect_error(get_efo_table(c("6.5", "1.7")), regexp = "single character")
  expect_error(get_efo_table(NULL),             regexp = "single character")
})

# ----- Cross-reference and complex-layout sheets ---------------------------

test_that("get_efo_table() follows cross-references to a previous EFO", {
  skip_on_cran()
  skip_if_offline()
  out <- get_efo_table("6.11")
  expect_s3_class(out, "obr_tbl")
  expect_named(out, v04_long_cols)
  prov <- obr_provenance(out)
  # Resolves to a previous EFO vintage, not the current one
  expect_match(prov$vintage, "^[A-Z][a-z]+ [0-9]{4}$")
  expect_match(prov$notes, "Cross-reference")
})

test_that("get_efo_table('6.4') returns subsector_matrix shape", {
  skip_on_cran()
  skip_if_offline()
  out <- get_efo_table("6.4")
  expect_s3_class(out, "obr_tbl")
  expect_true("sub_sector" %in% names(out))
  expect_true(all(c(v04_long_cols, "sub_sector") %in% names(out)))
  expect_true("Central government" %in% out$sub_sector)
  expect_true(all(out$period_type == "fiscal_year"))
  expect_true(all(out$unit == "gbp_bn"))
})

test_that("get_efo_table('6.10') returns quarterly_indented data", {
  skip_on_cran()
  skip_if_offline()
  out <- get_efo_table("6.10")
  expect_s3_class(out, "obr_tbl")
  expect_named(out, v04_long_cols)
  expect_true(all(out$period_type == "quarter"))
  expect_true(all(grepl("^[0-9]{4}Q[1-4]$", out$period)))
  expect_true(all(out$unit == "pct"))
})

# ----- Each layout returns the standard schema -----------------------------

test_that("get_efo_table('6.5') returns standard schema (fiscal_year_wide)", {
  skip_on_cran()
  skip_if_offline()
  out <- get_efo_table("6.5")
  expect_s3_class(out, "obr_tbl")
  expect_named(out, v04_long_cols)
  expect_true(all(out$period_type == "fiscal_year"))
  expect_true(all(out$unit == "gbp_bn"))
  expect_true("Net borrowing" %in% out$series)
})

test_that("get_efo_table('1.7') returns standard schema (quarterly_wide)", {
  skip_on_cran()
  skip_if_offline()
  out <- get_efo_table("1.7")
  expect_s3_class(out, "obr_tbl")
  expect_named(out, v04_long_cols)
  expect_true(all(out$period_type == "quarter"))
  expect_true(all(out$metric_type == "yoy_pct"))
  expect_true(all(out$unit == "pct"))
  expect_true("CPI" %in% out$series)
})

test_that("get_efo_table('1.4') returns standard schema (quarterly_single)", {
  skip_on_cran()
  skip_if_offline()
  out <- get_efo_table("1.4")
  expect_s3_class(out, "obr_tbl")
  expect_named(out, v04_long_cols)
  expect_true(all(out$period_type == "quarter"))
  expect_true(all(out$series == "Nominal GDP"))
})

test_that("get_efo_table('1.13') returns standard schema (annual_year_wide)", {
  skip_on_cran()
  skip_if_offline()
  out <- get_efo_table("1.13")
  expect_s3_class(out, "obr_tbl")
  expect_named(out, v04_long_cols)
  expect_true(all(out$period_type == "calendar_year"))
})

test_that("get_efo_table('1.19b') returns standard schema (annual_period_wide)", {
  skip_on_cran()
  skip_if_offline()
  out <- get_efo_table("1.19b")
  expect_s3_class(out, "obr_tbl")
  expect_named(out, v04_long_cols)
  expect_true(all(out$period_type == "calendar_year"))
})

# ----- Backward compatibility: headline wrappers still work ----------------

test_that("get_efo_fiscal() == get_efo_table('6.5')", {
  skip_on_cran()
  skip_if_offline()
  via_fn  <- get_efo_fiscal()
  via_id  <- get_efo_table("6.5")
  # Same data shape, same series, same values
  expect_identical(names(via_fn), names(via_id))
  expect_equal(nrow(via_fn), nrow(via_id))
  expect_setequal(via_fn$series, via_id$series)
})

test_that("get_efo_economy('inflation') == get_efo_table('1.7')", {
  skip_on_cran()
  skip_if_offline()
  via_fn <- get_efo_economy("inflation")
  via_id <- get_efo_table("1.7")
  expect_identical(names(via_fn), names(via_id))
  expect_equal(nrow(via_fn), nrow(via_id))
})

test_that("get_efo_economy('output_gap') keeps series='Output gap'", {
  skip_on_cran()
  skip_if_offline()
  out <- get_efo_economy("output_gap")
  expect_true(all(out$series == "Output gap"))
})

# ----- Index-linked gilts edge case (the v0.5.0 classifier fix) ------------

test_that("Net debt composition (6.13) classifies all rows as pct", {
  # Regression test for the v0.5.0 classifier fix. "Index-linked gilts" was
  # being misclassified as metric_type=index because the bare \\bindex\\b
  # pattern matched. Now the pattern requires Index at end of string.
  skip_on_cran()
  skip_if_offline()
  out <- get_efo_table("6.13")
  expect_true(all(out$metric_type == "pct"))
  expect_true(all(out$unit == "pct"))
  expect_true("Index-linked gilts" %in% out$series)
})
