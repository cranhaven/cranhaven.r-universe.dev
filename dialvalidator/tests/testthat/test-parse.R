test_that("parse E164 numbers", {
  result <- phone_parse("+64211234567")
  expect_length(result, 1)
  p <- result[[1]]
  expect_equal(p$country_code, "64")
  expect_equal(p$national_number, "211234567")
  expect_equal(p$region, "NZ")
  expect_true(p$valid)
})

test_that("parse US number", {
  p <- phone_parse("+12125551234")[[1]]
  expect_equal(p$country_code, "1")
  expect_equal(p$national_number, "2125551234")
  expect_equal(p$region, "US")
  expect_true(p$valid)
})

test_that("parse UK number", {
  p <- phone_parse("+442071234567")[[1]]
  expect_equal(p$country_code, "44")
  expect_equal(p$region, "GB")
  expect_true(p$valid)
})

test_that("parse national format with default_region", {
  p <- phone_parse("021 123 4567", default_region = "NZ")[[1]]
  expect_equal(p$country_code, "64")
  expect_equal(p$national_number, "211234567")
  expect_equal(p$region, "NZ")
  expect_true(p$valid)
})

test_that("national format without default_region fails", {
  p <- phone_parse("021 123 4567")[[1]]
  expect_false(p$valid)
})

test_that("parse vectorised input", {
  results <- phone_parse(c("+64211234567", "+12125551234", "+442071234567"))
  expect_length(results, 3)
  expect_true(all(vapply(results, function(p) p$valid, logical(1))))
  expect_equal(
    vapply(results, function(p) p$region, character(1)),
    c("NZ", "US", "GB")
  )
})

test_that("NANPA resolution: US vs CA", {
  # US number (212 is NYC)
  us <- phone_parse("+12125551234")[[1]]
  expect_equal(us$region, "US")

  # CA number (416 is Toronto)
  ca <- phone_parse("+14165551234")[[1]]
  expect_equal(ca$region, "CA")
})

test_that("parse preserves raw input", {
  p <- phone_parse("+64 21 123 4567")[[1]]
  expect_equal(p$raw, "+64 21 123 4567")
})

test_that("parse strips non-digit chars", {
  p <- phone_parse("+64-21-123-4567")[[1]]
  expect_equal(p$national_number, "211234567")
  expect_true(p$valid)
})

test_that("parse AU mobile", {
  p <- phone_parse("+61412345678")[[1]]
  expect_equal(p$country_code, "61")
  expect_equal(p$region, "AU")
  expect_true(p$valid)
})
