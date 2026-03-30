test_that("get_psnb() returns correct structure", {
  skip_on_cran()
  skip_if_offline()

  result <- get_psnb()

  expect_s3_class(result, "data.frame")
  expect_named(result, c("year", "psnb_bn"))
  expect_type(result$year, "character")
  expect_type(result$psnb_bn, "double")
  expect_gt(nrow(result), 50)
  # Fiscal year format
  expect_true(all(grepl("^[0-9]{4}-[0-9]{2}$", result$year)))
})

test_that("get_psnd() returns correct structure", {
  skip_on_cran()
  skip_if_offline()

  result <- get_psnd()

  expect_s3_class(result, "data.frame")
  expect_named(result, c("year", "psnd_bn"))
  expect_type(result$psnd_bn, "double")
  expect_gt(nrow(result), 30)
})

test_that("get_expenditure() returns correct structure", {
  skip_on_cran()
  skip_if_offline()

  result <- get_expenditure()

  expect_s3_class(result, "data.frame")
  expect_named(result, c("year", "tme_bn"))
  expect_type(result$tme_bn, "double")
  expect_gt(nrow(result), 50)
})

test_that("get_receipts() returns correct structure", {
  skip_on_cran()
  skip_if_offline()

  result <- get_receipts()

  expect_s3_class(result, "data.frame")
  expect_named(result, c("year", "series", "value"))
  expect_type(result$series, "character")
  expect_type(result$value, "double")
  # Should have multiple tax series
  expect_gt(length(unique(result$series)), 10)
  # Should include income tax and VAT
  expect_true(any(grepl("income tax", result$series, ignore.case = TRUE)))
  expect_true(any(grepl("VAT", result$series, ignore.case = TRUE)))
})

test_that("get_public_finances() returns all aggregate series", {
  skip_on_cran()
  skip_if_offline()

  result <- get_public_finances()

  expect_s3_class(result, "data.frame")
  expect_named(result, c("year", "series", "value"))
  # Should include PSNB and PSND
  series <- unique(result$series)
  expect_true("Public sector net borrowing" %in% series)
  expect_true("Public sector net debt" %in% series)
  expect_true("Total managed expenditure" %in% series)
})

test_that("COVID-19 spike visible in PSNB data", {
  skip_on_cran()
  skip_if_offline()

  psnb <- get_psnb()
  covid_year <- psnb$psnb_bn[psnb$year == "2020-21"]
  normal_year <- psnb$psnb_bn[psnb$year == "2018-19"]

  expect_gt(covid_year, normal_year * 5)
})
