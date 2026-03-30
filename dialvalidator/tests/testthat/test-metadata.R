test_that("metadata loads and has expected structure", {
  meta <- dv_metadata()
  expect_type(meta, "list")
  expect_named(meta, c("cc_to_regions", "territories", "example_numbers",
                        "version", "built"), ignore.order = TRUE)
  expect_gt(length(meta$territories), 240)
  expect_gt(length(meta$cc_to_regions), 200)
})

test_that("NZ territory has correct structure", {
  nz <- dv_territory("NZ")
  expect_equal(nz$id, "NZ")
  expect_equal(nz$country_code, "64")
  expect_equal(nz$national_prefix, "0")
  expect_type(nz$mobile, "list")
  expect_type(nz$mobile$pattern, "character")
  expect_type(nz$mobile$possible_lengths, "integer")
  expect_type(nz$mobile$example, "character")
  expect_type(nz$fixed_line, "list")
  expect_type(nz$formats, "list")
  expect_gt(length(nz$formats), 0)
})

test_that("US territory has correct structure", {
  us <- dv_territory("US")
  expect_equal(us$country_code, "1")
  expect_true(isTRUE(us$main_country_for_code))
  expect_type(us$mobile, "list")
  expect_type(us$toll_free, "list")
})

test_that("GB and AU territories exist", {
  gb <- dv_territory("GB")
  expect_equal(gb$country_code, "44")
  au <- dv_territory("AU")
  expect_equal(au$country_code, "61")
})

test_that("cc_to_regions maps shared codes correctly", {
  meta <- dv_metadata()
  # +1 is shared by US, CA, and many Caribbean nations
  nanpa <- meta$cc_to_regions[["1"]]
  expect_true("US" %in% nanpa)
  expect_true("CA" %in% nanpa)
  # US should be first (main country)
  expect_equal(nanpa[1], "US")
})

test_that("dv_territory returns NULL for unknown region", {
  expect_null(dv_territory("ZZ"))
  expect_null(dv_territory("XX"))
})

test_that("case insensitive region lookup", {
  expect_equal(dv_territory("nz")$id, "NZ")
  expect_equal(dv_territory("Nz")$id, "NZ")
})

test_that("example numbers are extracted", {
  meta <- dv_metadata()
  expect_gt(length(meta$example_numbers), 500)
  ex <- meta$example_numbers[[1]]
  expect_named(ex, c("region", "country_code", "type", "example"),
               ignore.order = TRUE)
})
