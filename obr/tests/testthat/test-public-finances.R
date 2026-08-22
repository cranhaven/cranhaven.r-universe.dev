# v0.4.0 standard tidy long schema for all data-fetching functions.
v04_long_cols <- c("period", "period_type", "series",
                   "metric_type", "value", "unit")

test_that("get_psnb() returns the v0.4.0 schema with series='PSNB'", {
  skip_on_cran()
  skip_if_offline()

  result <- get_psnb()

  expect_s3_class(result, "data.frame")
  expect_named(result, v04_long_cols)
  expect_type(result$period, "character")
  expect_type(result$value,  "double")
  expect_gt(nrow(result), 50)
  expect_true(all(grepl("^[0-9]{4}-[0-9]{2}$", result$period)))
  expect_true(all(result$series == "PSNB"))
  expect_true(all(result$period_type == "fiscal_year"))
  expect_true(all(result$metric_type == "level"))
  expect_true(all(result$unit == "gbp_bn"))
})

test_that("get_psnd() returns the v0.4.0 schema with series='PSND'", {
  skip_on_cran()
  skip_if_offline()

  result <- get_psnd()

  expect_s3_class(result, "data.frame")
  expect_named(result, v04_long_cols)
  expect_type(result$value, "double")
  expect_gt(nrow(result), 30)
  expect_true(all(result$series == "PSND"))
})

test_that("get_expenditure() returns the v0.4.0 schema with series='TME'", {
  skip_on_cran()
  skip_if_offline()

  result <- get_expenditure()

  expect_s3_class(result, "data.frame")
  expect_named(result, v04_long_cols)
  expect_type(result$value, "double")
  expect_gt(nrow(result), 50)
  expect_true(all(result$series == "TME"))
})

test_that("get_receipts() returns the v0.4.0 schema with multiple tax series", {
  skip_on_cran()
  skip_if_offline()

  result <- get_receipts()

  expect_s3_class(result, "data.frame")
  expect_named(result, v04_long_cols)
  expect_type(result$series, "character")
  expect_type(result$value,  "double")
  expect_gt(length(unique(result$series)), 10)
  expect_true(any(grepl("income tax", result$series, ignore.case = TRUE)))
  expect_true(any(grepl("VAT",        result$series, ignore.case = TRUE)))
  expect_true(all(result$unit == "gbp_bn"))
})

test_that("get_public_finances() returns the v0.4.0 schema with all aggregates", {
  skip_on_cran()
  skip_if_offline()

  result <- get_public_finances()

  expect_s3_class(result, "data.frame")
  expect_named(result, v04_long_cols)
  series <- unique(result$series)
  expect_true("Public sector net borrowing" %in% series)
  expect_true("Public sector net debt"      %in% series)
  expect_true("Total managed expenditure"   %in% series)
})

test_that("COVID-19 spike visible in PSNB data", {
  skip_on_cran()
  skip_if_offline()

  psnb <- get_psnb()
  covid_year  <- psnb$value[psnb$period == "2020-21"]
  normal_year <- psnb$value[psnb$period == "2018-19"]

  expect_gt(covid_year, normal_year * 5)
})

test_that("PFD-backed functions return obr_tbl with PFD provenance", {
  skip_on_cran()
  skip_if_offline()

  for (fn in list(get_psnb, get_psnd, get_expenditure,
                  get_receipts, get_public_finances)) {
    res  <- fn()
    expect_s3_class(res, "obr_tbl")
    prov <- obr_provenance(res)
    expect_equal(prov$publication, "PFD")
    expect_match(prov$source_url, "obr\\.uk")
    expect_true(inherits(prov$retrieved, "POSIXt"))
    expect_match(prov$file_md5, "^[a-f0-9]{32}$")
    expect_match(prov$package_version, "^[0-9]+\\.[0-9]+\\.[0-9]+")
  }
})
