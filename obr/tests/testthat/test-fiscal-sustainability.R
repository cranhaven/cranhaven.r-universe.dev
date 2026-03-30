test_that("get_pension_projections() returns correct structure", {
  skip_on_cran()
  skip_if_offline()

  result <- get_pension_projections()

  expect_s3_class(result, "data.frame")
  expect_named(result, c("scenario_type", "scenario", "fiscal_year", "pct_gdp"))
  expect_type(result$scenario_type, "character")
  expect_type(result$scenario,      "character")
  expect_type(result$fiscal_year,   "character")
  expect_type(result$pct_gdp,       "double")
  expect_gt(nrow(result), 100)
})

test_that("get_pension_projections() covers two scenario types", {
  skip_on_cran()
  skip_if_offline()

  result <- get_pension_projections()
  types  <- unique(result$scenario_type)

  expect_true("Demographic scenarios"  %in% types)
  expect_true("Triple lock scenarios"  %in% types)
})

test_that("get_pension_projections() covers 50-year horizon", {
  skip_on_cran()
  skip_if_offline()

  result  <- get_pension_projections()
  dem     <- result[result$scenario_type == "Demographic scenarios", ]
  n_years <- length(unique(dem$fiscal_year))

  expect_gte(n_years, 40)
  # Should reach at least 2060s
  expect_true(any(grepl("^206", dem$fiscal_year)))
})

test_that("get_pension_projections() has plausible pension spending values", {
  skip_on_cran()
  skip_if_offline()

  result  <- get_pension_projections()
  # State pension spending is typically 4-10% of GDP
  expect_true(all(result$pct_gdp >= 2 & result$pct_gdp <= 20))
  # Central demographic projection should be around 4-6% of GDP near the start
  central <- result[
    result$scenario == "Central projection" &
    result$scenario_type == "Demographic scenarios", ]
  expect_gt(nrow(central), 0)
  expect_true(all(central$pct_gdp >= 3 & central$pct_gdp <= 15))
})
