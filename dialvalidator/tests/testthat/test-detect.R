test_that("NZ mobile detected", {
  expect_equal(phone_type("+64211234567"), "mobile")
})

test_that("NZ landline detected", {
  # Use NZ fixed line example from metadata
  nz <- dv_territory("NZ")
  if (!is.null(nz$fixed_line$example)) {
    full <- paste0("+64", nz$fixed_line$example)
    result <- phone_type(full)
    expect_true(result %in% c("fixed_line", "fixed_line_or_mobile"))
  }
})

test_that("US toll-free detected", {
  result <- phone_type("+18005551234")
  # 800 numbers should be toll_free
  expect_equal(result, "toll_free")
})

test_that("phone_country returns correct regions", {
  expect_equal(phone_country("+64211234567"), "NZ")
  expect_equal(phone_country("+12125551234"), "US")
  expect_equal(phone_country("+442071234567"), "GB")
  expect_equal(phone_country("+61412345678"), "AU")
})

test_that("phone_country returns NA for invalid", {
  expect_true(is.na(phone_country("invalid")))
  expect_true(is.na(phone_country(NA_character_)))
})

test_that("phone_type returns NA for invalid", {
  expect_true(is.na(phone_type("invalid")))
})

test_that("vectorised type detection", {
  types <- phone_type(c("+64211234567", "+18005551234"))
  expect_equal(types[1], "mobile")
  expect_equal(types[2], "toll_free")
})

test_that("example numbers match their declared type", {
  meta <- dv_metadata()
  # Test mobile examples from a few key regions
  for (region_id in c("NZ", "US", "GB", "AU", "DE", "JP")) {
    territory <- meta$territories[[region_id]]
    if (!is.null(territory$mobile$example)) {
      full <- paste0("+", territory$country_code, territory$mobile$example)
      result <- phone_type(full)
      expect_true(
        result %in% c("mobile", "fixed_line_or_mobile"),
        label = sprintf("%s mobile example: %s -> %s", region_id, full, result)
      )
    }
  }
})
