# v0.4.0 standard tidy long schema.
v04_long_cols <- c("period", "period_type", "series",
                   "metric_type", "value", "unit")

test_that("get_welfare_spending() returns the v0.4.0 schema", {
  skip_on_cran()
  skip_if_offline()

  result <- get_welfare_spending()

  expect_s3_class(result, "data.frame")
  expect_named(result, v04_long_cols)
  expect_type(result$period, "character")
  expect_type(result$series, "character")
  expect_type(result$value,  "double")
  expect_gt(nrow(result), 50)
  expect_true(any(grepl("incapacity", result$series, ignore.case = TRUE)))
  expect_true(all(grepl("^[0-9]{4}-[0-9]{2}$", result$period)))
  # All values are percentages of GDP
  expect_true(all(result$value >= 0 & result$value < 20))
  expect_true(all(result$period_type == "fiscal_year"))
})

test_that("get_welfare_spending() covers from 1978-79", {
  skip_on_cran()
  skip_if_offline()

  result <- get_welfare_spending()
  expect_true("1978-79" %in% result$period)
})

test_that("get_incapacity_spending() returns the v0.4.0 schema", {
  skip_on_cran()
  skip_if_offline()

  result <- get_incapacity_spending()

  expect_s3_class(result, "data.frame")
  expect_named(result, v04_long_cols)
  expect_gt(nrow(result), 100)
  expect_true(any(grepl("ESA|Employment and support",
                        result$series, ignore.case = TRUE)))
  expect_true(any(grepl("Invalidity", result$series)))
})

test_that("get_incapacity_caseloads() returns the v0.4.0 schema with mixed units", {
  skip_on_cran()
  skip_if_offline()

  result <- get_incapacity_caseloads()

  expect_s3_class(result, "data.frame")
  expect_named(result, v04_long_cols)
  expect_gt(nrow(result), 20)
  expect_true("Claimants" %in% result$series)
  expect_true(any(grepl("working age", result$series, ignore.case = TRUE)))
  claimants <- result[result$series == "Claimants", ]
  expect_true("2008-09" %in% claimants$period)
  # Claimants in thousands; roughly 2,000 to 3,500 for incapacity
  expect_true(all(claimants$value > 500 & claimants$value < 10000))
  # Wrapper retags the claimants series with the right unit
  expect_true(all(claimants$unit == "count_k"))
})

test_that("WTR functions return obr_tbl with WTR provenance", {
  skip_on_cran()
  skip_if_offline()

  for (fn in list(get_welfare_spending, get_incapacity_spending,
                  get_incapacity_caseloads)) {
    res  <- fn()
    expect_s3_class(res, "obr_tbl")
    prov <- obr_provenance(res)
    expect_equal(prov$publication, "WTR")
    expect_match(prov$source_url, "welfare-trends-report")
    expect_match(prov$vintage, "^[A-Z][a-z]+ [0-9]{4}$")
  }
})
