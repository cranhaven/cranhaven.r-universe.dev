test_that("agency accepts character vector of length > 1 as input", {
  withr::local_options(ntdr.cache = TRUE)

  expect_no_error(
    get_ntd(agency = c(
      "City of Madison",
      "Capital Area Transportation Authority"
    ))
  )
})

test_that("column names are correct", {
  withr::local_options(ntdr.cache = TRUE)

  expect_equal(
    colnames(get_ntd()),
    c(
      "ntd_id_5",
      "ntd_id_4",
      "agency",
      "active",
      "reporter_type",
      "uace",
      "uza_name",
      "modes",
      "tos",
      "modes_simplified",
      "month",
      "value",
      "ntd_variable"
    )
  )
})

test_that("ntd variable has correct value", {
  withr::local_options(ntdr.cache = TRUE)

  x <- get_ntd()
  y <- get_ntd(ntd_variable = "VRM")
  expect_equal(x$ntd_variable[[1]], "UPT")
  expect_equal(y$ntd_variable[[1]], "VRM")
})

test_that("returns a URL that starts right", {
  expect_match(get_ntd_url(), regexp = "^https://www.transit.dot.gov/sites/fta.dot.gov/files/")
})

test_that("URL contains the right terms for different data_types", {
  expect_match(get_ntd_url("raw"), regexp = "Raw")
  expect_match(get_ntd_url("adjusted"), regexp = "Complete")
})

test_that("function returns error for invalid parameter values", {
  expect_error(get_ntd_url("nonsense"))
})

test_that("a specific ridership value is correct", {
  withr::local_options(ntdr.cache = TRUE)

  x <- get_ntd(
    agency = "City of Madison",
    modes = "MB"
  )
  y <- x |>
    dplyr::filter(as.character(month) == "2002-01-01") |>
    dplyr::pull(value)
  expect_equal(y, 865836)
})
