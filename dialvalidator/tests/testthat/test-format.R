test_that("E164 formatting", {
  expect_equal(phone_format("+64211234567"), "+64211234567")
  expect_equal(phone_format("+64 21 123 4567"), "+64211234567")
  expect_equal(phone_format("+1 212 555 1234"), "+12125551234")
})

test_that("E164 from national format", {
  expect_equal(
    phone_format("021 123 4567", default_region = "NZ"),
    "+64211234567"
  )
})

test_that("NATIONAL formatting NZ mobile", {
  result <- phone_format("+64211234567", "NATIONAL")
  expect_type(result, "character")
  expect_false(is.na(result))
  # Should contain digits in some grouped format
  digits <- gsub("[^0-9]", "", result)
  expect_true(nchar(digits) >= 9) # national number + prefix
})

test_that("INTERNATIONAL formatting", {
  result <- phone_format("+64211234567", "INTERNATIONAL")
  expect_true(startsWith(result, "+64"))
  expect_true(grepl(" ", result)) # should have spaces
})

test_that("invalid number returns NA", {
  expect_true(is.na(phone_format("invalid")))
  expect_true(is.na(phone_format("")))
  expect_true(is.na(phone_format(NA_character_)))
})

test_that("vectorised formatting", {
  result <- phone_format(c("+64211234567", "+12125551234"), "E164")
  expect_equal(result, c("+64211234567", "+12125551234"))
})

test_that("format argument matching", {
  expect_error(phone_format("+64211234567", "INVALID"))
})
