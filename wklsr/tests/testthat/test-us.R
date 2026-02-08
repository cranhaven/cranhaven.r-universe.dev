test_that("basic access patterns work correctly", {
  skip_if_offline()

  # Test country level - just verify it's a MULTIPOLYGON
  us_wkt <- wkls$us$wkt()
  expect_type(us_wkt, "character")
  expect_true(startsWith(us_wkt, "MULTIPOLYGON"))
  expect_true(nchar(us_wkt) > 100)  # Should be substantial geometry

  # Test region level
  ca_wkt <- wkls$us$ca$wkt()
  expect_type(ca_wkt, "character")
  expect_true(startsWith(ca_wkt, "MULTIPOLYGON"))

  # Test city level - New York
  ny_newyork_wkt <- wkls$us$ny$newyork$wkt()
  expect_type(ny_newyork_wkt, "character")
  expect_true(grepl("POLYGON", ny_newyork_wkt))  # Could be POLYGON or MULTIPOLYGON

  # Test city level - City of New York
  cityofnewyork_wkt <- wkls$us$ny$cityofnewyork$wkt()
  expect_type(cityofnewyork_wkt, "character")
  expect_true(grepl("POLYGON", cityofnewyork_wkt))

  # Test city level - San Francisco
  sf_wkt <- wkls$us$ca$sanfrancisco$wkt()
  expect_type(sf_wkt, "character")
  expect_true(grepl("POLYGON", sf_wkt))
})

test_that("helper functions return correct counts", {
  skip_if_offline()

  # Test countries count
  countries <- wkls$countries()
  expect_equal(nrow(countries), 219)

  # Test US regions count
  us_regions <- wkls$us$regions()
  expect_equal(nrow(us_regions), 51)

  # Test India Maharashtra counties
  in_mh_counties <- wkls[["IN"]][["MH"]]$counties()
  expect_equal(nrow(in_mh_counties), 36)

  # Test India Maharashtra cities
  in_mh_cities <- wkls[["IN"]][["MH"]]$cities()
  expect_equal(nrow(in_mh_cities), 327)
})

test_that("pattern search with % returns data frame directly", {
  skip_if_offline()

  # Test San Francisco search returns data.frame directly
  san_francisco_results <- wkls[["us"]][["ca"]][["%San Francisco%"]]

  expect_true(is.data.frame(san_francisco_results))
  expect_equal(nrow(san_francisco_results), 2,
               info = "San Francisco search should return exactly two results")

  # Check that results contain San Francisco in the name
  expect_true(any(grepl("San Francisco", san_francisco_results$name_primary, ignore.case = TRUE)),
              info = "Results should contain San Francisco")
})

test_that("subtypes returns all expected division types", {
  skip_if_offline()

  subtypes_df <- wkls$subtypes()
  expect_true(is.data.frame(subtypes_df))

  expected_subtypes <- c("country", "region", "county", "locality", "localadmin")

  for (subtype in expected_subtypes) {
    expect_true(subtype %in% subtypes_df$subtype,
                info = sprintf("Subtype '%s' should exist", subtype))
  }
})

test_that("overture_version is accessible and returns correct version", {
  skip_if_offline()

  # Should work at root level
  version <- wkls$overture_version()
  expect_type(version, "character")
  expect_true(grepl("2025-11-19.0", version),
              info = "Version should contain 2025-11-19.0")

  message(sprintf("Using Overture Maps dataset version: %s", version))

  # Should NOT work on chained objects - should throw error
  expect_error(
    wkls$us$overture_version(),
    "overture_version\\(\\) is only available at the root level",
    info = "Correctly blocked chained access: wkls$us should not have overture_version method"
  )

  message("Correctly blocked chained access: wkls$us does not have overture_version method")
})
