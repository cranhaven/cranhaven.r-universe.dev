test_that("get_welfare_spending() returns correct structure", {
  skip_on_cran()
  skip_if_offline()

  result <- get_welfare_spending()

  expect_s3_class(result, "data.frame")
  expect_named(result, c("year", "series", "value"))
  expect_type(result$year,   "character")
  expect_type(result$series, "character")
  expect_type(result$value,  "double")
  expect_gt(nrow(result), 50)
  expect_true(any(grepl("incapacity", result$series, ignore.case = TRUE)))
  expect_true(all(grepl("^[0-9]{4}-[0-9]{2}$", result$year)))
  # All values are percentages of GDP — should be small positive numbers
  expect_true(all(result$value >= 0 & result$value < 20))
})

test_that("get_welfare_spending() covers from 1978-79", {
  skip_on_cran()
  skip_if_offline()

  result <- get_welfare_spending()
  expect_true("1978-79" %in% result$year)
})

test_that("get_incapacity_spending() returns correct structure", {
  skip_on_cran()
  skip_if_offline()

  result <- get_incapacity_spending()

  expect_s3_class(result, "data.frame")
  expect_named(result, c("year", "series", "value"))
  expect_gt(nrow(result), 100)
  # Should include ESA and historical benefit types
  expect_true(any(grepl("ESA|Employment and support", result$series, ignore.case = TRUE)))
  expect_true(any(grepl("Invalidity", result$series)))
})

test_that("get_incapacity_caseloads() returns correct structure", {
  skip_on_cran()
  skip_if_offline()

  result <- get_incapacity_caseloads()

  expect_s3_class(result, "data.frame")
  expect_named(result, c("year", "series", "value"))
  expect_gt(nrow(result), 20)
  expect_true("Claimants" %in% result$series)
  expect_true(any(grepl("working age", result$series, ignore.case = TRUE)))
  # Claimants should start from around 2008-09
  claimants <- result[result$series == "Claimants", ]
  expect_true("2008-09" %in% claimants$year)
  # Values should be in thousands — roughly 2,000–3,500 for incapacity claimants
  expect_true(all(claimants$value > 500 & claimants$value < 10000))
})
