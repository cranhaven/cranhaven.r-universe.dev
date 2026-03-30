test_that("valid numbers return TRUE", {
  expect_true(phone_valid("+64211234567"))
  expect_true(phone_valid("+12125551234"))
  expect_true(phone_valid("+442071234567"))
  expect_true(phone_valid("+61412345678"))
})

test_that("invalid numbers return FALSE", {
  expect_false(phone_valid("+6421"))        # too short
  expect_false(phone_valid("+640000000"))   # invalid pattern
  expect_false(phone_valid("not a number"))
  expect_false(phone_valid(""))
  expect_false(phone_valid(NA_character_))
})

test_that("national format with default_region validates", {
  expect_true(phone_valid("021 123 4567", default_region = "NZ"))
  expect_true(phone_valid("(212) 555-1234", default_region = "US"))
})

test_that("vectorised validation", {
  result <- phone_valid(c("+64211234567", "invalid", "+12125551234"))
  expect_equal(result, c(TRUE, FALSE, TRUE))
})

test_that("example numbers from metadata validate", {
  meta <- dv_metadata()
  # Test a sample of example numbers
  examples <- meta$example_numbers
  # Take first 50 examples
  sample_size <- min(50, length(examples))
  sample_examples <- examples[seq_len(sample_size)]

  for (ex in sample_examples) {
    full_number <- paste0("+", ex$country_code, ex$example)
    result <- phone_valid(full_number)
    expect_true(
      result,
      label = sprintf("%s (%s, %s)", full_number, ex$region, ex$type)
    )
  }
})
