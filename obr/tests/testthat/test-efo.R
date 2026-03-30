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
    regexp = "Unknown measure"
  )
})

test_that("get_efo_fiscal() returns correct structure", {
  skip_on_cran()
  skip_if_offline()

  result <- get_efo_fiscal()

  expect_s3_class(result, "data.frame")
  expect_named(result, c("fiscal_year", "series", "value_bn"))
  expect_type(result$fiscal_year, "character")
  expect_type(result$series,      "character")
  expect_type(result$value_bn,    "double")
  expect_gt(nrow(result), 10)
  expect_true("Net borrowing" %in% result$series)
  expect_true("Current receipts" %in% result$series)
})

test_that("get_efo_fiscal() covers 5-year forecast horizon", {
  skip_on_cran()
  skip_if_offline()

  result <- get_efo_fiscal()
  years  <- unique(result$fiscal_year)

  expect_gte(length(years), 5)
  # All fiscal years should look like "YYYY-YY"
  expect_true(all(grepl("^[0-9]{4}-[0-9]{2}$", years)))
})

test_that("get_efo_economy('inflation') returns correct structure", {
  skip_on_cran()
  skip_if_offline()

  result <- get_efo_economy("inflation")

  expect_s3_class(result, "data.frame")
  expect_named(result, c("period", "series", "value"))
  expect_type(result$period, "character")
  expect_type(result$series, "character")
  expect_type(result$value,  "double")
  expect_gt(nrow(result), 200)
  expect_true("CPI"  %in% result$series)
  expect_true("CPIH" %in% result$series)
  # Periods should be quarterly
  expect_true(all(grepl("^[0-9]{4}Q[1-4]$", result$period)))
})

test_that("get_efo_economy('labour') returns correct structure", {
  skip_on_cran()
  skip_if_offline()

  result <- get_efo_economy("labour")

  expect_s3_class(result, "data.frame")
  expect_named(result, c("period", "series", "value"))
  expect_gt(nrow(result), 200)
  expect_true(any(grepl("Employment", result$series)))
  expect_true(any(grepl("unemployment", result$series, ignore.case = TRUE)))
})

test_that("get_efo_economy('output_gap') returns correct structure", {
  skip_on_cran()
  skip_if_offline()

  result <- get_efo_economy("output_gap")

  expect_s3_class(result, "data.frame")
  expect_named(result, c("period", "series", "value"))
  expect_gt(nrow(result), 100)
  expect_true(all(result$series == "Output gap"))
  # Output gap should span from 1970s onwards
  expect_true(any(grepl("^197", result$period)))
})
