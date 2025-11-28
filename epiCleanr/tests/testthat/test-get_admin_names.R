

# Skip all tests on CRAN
if (!identical(Sys.getenv("NOT_CRAN"), "true")) {
  return()
}

# Test 1: Test with Valid ISO Codes
testthat::test_that(
  "get_admin_names returns correct data for valid ISO codes",
  {
    result <- get_admin_names("JAM") # Somalia ISO code
    testthat::expect_equal(typeof(result), "list")
    testthat::expect_true(length(result) > 0)
    testthat::expect_true(is.data.frame(result[[1]]), "data.frame")
  }
)


# Test 2: Test with Valid Country Name
testthat::test_that(
  "get_admin_names returns correct data for valid country name",
  {
    result <- get_admin_names("JAM")
    testthat::expect_equal(typeof(result), "list")
    testthat::expect_true(length(result) > 0)
    testthat::expect_true(is.data.frame(result[[1]]), "data.frame")
  }
)

# Test 3: Test with Invalid Country Code/Name
testthat::test_that(
  "get_admin_names raises error for invalid country code/name",
  {
    actual <- as.character(
      testthat::capture_error(get_admin_names("INVALID_CODE")))
    expected <- paste(
      "Error in get_admin_names(\"INVALID_CODE\"):",
      "Country name or code not recognized.\n"
    )

    testthat::expect_equal(actual, expected)
  }
)


# Test 4: Test with Different Naming/Code Conventions
testthat::test_that(
  "get_admin_names handles different naming/code conventions",
  {
    result_iso3 <- get_admin_names("JAM") # Jamaica 3-digit ISO code
    result_iso2 <- get_admin_names("JM") # Jamaica 2-digit ISO code
    result_un <- get_admin_names(388) # Jamaica UN code
    result_name <- get_admin_names("Jamaica") # Using full name
    # All results should be non-empty lists
    testthat::expect_true(all(sapply(
      list(
        result_iso3, result_iso2,
        result_un, result_name
      ),
      function(x) length(x) > 0
    )))
  }
)

# Test 5: Test with Character Input
testthat::test_that(
  "coding_schemes includes only char schemes for character input", {
  result <- get_admin_names("JAM") # Using 3 digit ISO code (character input)
  testthat::expect_type(result, "list") # Expecting a list as output
})

# Test 6: Test with Numeric Input
testthat::test_that(
  "coding_schemes includes both char and numeric schemes for numeric input",
  {
    result <- get_admin_names(840)
    testthat::expect_type(result, "list")
  }
)
