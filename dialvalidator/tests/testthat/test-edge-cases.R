test_that("NA input handled", {
  expect_false(phone_valid(NA_character_))
  expect_true(is.na(phone_format(NA_character_)))
  expect_true(is.na(phone_type(NA_character_)))
  expect_true(is.na(phone_country(NA_character_)))
})
test_that("empty string handled", {
  expect_false(phone_valid(""))
  expect_true(is.na(phone_format("")))
})

test_that("NULL input handled", {
  result <- phone_parse(character(0))
  expect_length(result, 0)
})

test_that("very long number is invalid", {
  long_num <- paste0("+64", paste(rep("1", 20), collapse = ""))
  expect_false(phone_valid(long_num))
})

test_that("too short number is invalid", {
  expect_false(phone_valid("+641"))
  expect_false(phone_valid("+6421"))
})

test_that("all zeros is invalid", {
  expect_false(phone_valid("+640000000"))
})

test_that("phone_info returns data frame", {
  result <- phone_info("+64211234567")
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 1)
  expect_named(result, c("raw", "e164", "national", "international",
                          "region", "country_code", "type", "valid"),
               ignore.order = TRUE)
  expect_equal(result$e164, "+64211234567")
  expect_equal(result$region, "NZ")
  expect_true(result$valid)
})

test_that("phone_info with multiple numbers", {
  result <- phone_info(c("+64211234567", "+12125551234", "invalid"))
  expect_equal(nrow(result), 3)
  expect_equal(result$valid, c(TRUE, TRUE, FALSE))
  expect_true(is.na(result$e164[3]))
})

test_that("numbers with special characters parsed correctly", {
  # Parentheses, dashes, dots, spaces
  expect_true(phone_valid("+64-21-123-4567"))
  expect_true(phone_valid("+64.21.123.4567"))
  expect_true(phone_valid("+64 21 123 4567"))
})

test_that("number with only country code is invalid", {
  expect_false(phone_valid("+64"))
  expect_false(phone_valid("+1"))
})
