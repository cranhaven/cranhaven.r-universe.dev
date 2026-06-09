# Tests for kfa_county_bbox() and kfa_list_counties()

test_that("kfa_list_counties() returns 120 counties", {
  counties <- kfa_list_counties()

  expect_type(counties, "character")
  expect_equal(length(counties), 120)
  expect_true("Fayette" %in% counties)
  expect_true("Jefferson" %in% counties)
})

test_that("kfa_county_bbox() returns valid bbox", {
  bbox <- kfa_county_bbox("Fayette")

  expect_type(bbox, "double")
  expect_length(bbox, 4)
  # xmin < xmax, ymin < ymax

  expect_true(bbox[1] < bbox[3])
  expect_true(bbox[2] < bbox[4])
  # Should be in Kentucky (roughly)
  expect_true(bbox[1] > -90 && bbox[3] < -81)
  expect_true(bbox[2] > 36 && bbox[4] < 40)
})

test_that("kfa_county_bbox() is case-insensitive", {
  expect_equal(kfa_county_bbox("fayette"), kfa_county_bbox("Fayette"))
})

test_that("kfa_county_bbox() errors on unknown county", {
  expect_error(kfa_county_bbox("Nonexistent"), "not found")
})

test_that("kfa_county_bbox() errors on ambiguous match", {
  # "Green" matches Green and Greenup
  expect_error(kfa_county_bbox("Green"), "Multiple")
})

test_that("kfa_county_bbox() validates input", {
  expect_error(kfa_county_bbox(123), "character")
  expect_error(kfa_county_bbox(c("Fayette", "Clark")), "single")
})
