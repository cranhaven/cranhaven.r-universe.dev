test_that("API base function properly", {
  # Test error cases without VCR since they don't make API calls
  expect_error(dawa(section = "base"))
  expect_error(dawa(section = "supermarked"))
})

test_that("Compare sogne API results", {
  skip_on_cran()
  old_result <- httr2::request("https://api.dataforsyningen.dk/sogne") |>
    httr2::req_perform() |>
    httr2::resp_body_json()

  new_result <- dawa("sogne", verbose = FALSE)

  expect_equal(old_result[[1]], new_result[[1]])
})

vcr::use_cassette("regioner_api_default", {
  test_that("Test regioner default", {
    expect_snapshot(dawa("regioner"))
  })
})

vcr::use_cassette("regioner_api_no_cache", {
  test_that("Test regioner no cache", {
    expect_snapshot(dawa("regioner", cache = FALSE))
  })
})

test_that("Test invalid append", {
  expect_error(dawa(
    section = "regioner",
    append_to_url = 1234
  ))
})
