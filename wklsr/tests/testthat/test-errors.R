test_that("countries without regions raise appropriate error", {
  skip_if_offline()

  expect_error(
    wkls$fk$regions(),
    "The country 'FK' does not have regions in the dataset",
    info = "FK (Falkland Islands) should not have regions"
  )
})

test_that("empty chain raises error", {
  skip_if_offline()

  # Create a proxy with empty chain
  empty_proxy <- .make_wkls_proxy(character(0))

  expect_error(
    .resolve_chain(character(0)),
    "No attributes in the chain",
    info = "Empty chain should raise error mentioning wkls$country"
  )
})

test_that("dependencies() cannot be called on chained objects", {
  skip_if_offline()

  expect_error(
    wkls$us$dependencies(),
    "dependencies\\(\\) can only be called on the root object",
    info = "dependencies() should only work at root level"
  )
})

test_that("countries() cannot be called on chained objects", {
  skip_if_offline()

  expect_error(
    wkls$us$countries(),
    "countries\\(\\) can only be called on the root object",
    info = "countries() should only work at root level"
  )
})

test_that("regions() requires exactly one level of chaining", {
  skip_if_offline()

  # regions() on root should fail
  expect_error(
    wkls$regions(),
    "regions\\(\\) requires exactly one level of chaining",
    info = "regions() should not work at root level"
  )

  # regions() on country.region should fail
  expect_error(
    wkls$us$ca$regions(),
    "regions\\(\\) requires exactly one level of chaining",
    info = "regions() should not work at region level"
  )
})

test_that("counties() requires exactly two levels of chaining", {
  skip_if_offline()

  # counties() on root should fail
  expect_error(
    wkls$counties(),
    "counties\\(\\) requires exactly two levels of chaining",
    info = "counties() should not work at root level"
  )

  # counties() on country only should fail
  expect_error(
    wkls$us$counties(),
    "counties\\(\\) requires exactly two levels of chaining",
    info = "counties() should not work with only country"
  )

  # counties() on country.region.city should fail
  expect_error(
    wkls$us$ca$sanfrancisco$counties(),
    "counties\\(\\) requires exactly two levels of chaining",
    info = "counties() should not work at city level"
  )
})

test_that("cities() requires exactly two levels of chaining", {
  skip_if_offline()

  # cities() on root should fail
  expect_error(
    wkls$cities(),
    "cities\\(\\) requires exactly two levels of chaining",
    info = "cities() should not work at root level"
  )

  # cities() on country only should fail
  expect_error(
    wkls$us$cities(),
    "cities\\(\\) requires exactly two levels of chaining",
    info = "cities() should not work with only country"
  )

  # cities() on country.region.city should fail
  expect_error(
    wkls$us$ca$sanfrancisco$cities(),
    "cities\\(\\) requires exactly two levels of chaining",
    info = "cities() should not work at city level"
  )
})

test_that("subtypes() cannot be called on chained objects", {
  skip_if_offline()

  expect_error(
    wkls$us$subtypes(),
    "subtypes\\(\\) can only be called on the root object",
    info = "subtypes() should only work at root level"
  )
})

test_that("too many chained attributes raise error", {
  skip_if_offline()

  expect_error(
    wkls$us$ca$sanfrancisco$somethingelse,
    "Too many chained attributes \\(max = 3\\)",
    info = "Should not allow more than 3 levels of chaining"
  )
})

test_that("nonexistent locations handle gracefully", {
  skip_if_offline()

  # Nonexistent country - should return empty data frame
  result <- tryCatch({
    zz_result <- wkls$zz
    print(zz_result)
    zz_result
  }, error = function(e) {
    # If it errors, that's also acceptable
    data.frame()
  })

  if (is.data.frame(result)) {
    expect_equal(nrow(result), 0,
                 info = "Nonexistent country should return empty data frame")
  }

  # Nonexistent region - should return empty data frame
  result <- tryCatch({
    us_zz_result <- wkls$us$zz
    print(us_zz_result)
    us_zz_result
  }, error = function(e) {
    # If it errors, that's also acceptable
    data.frame()
  })

  if (is.data.frame(result)) {
    expect_equal(nrow(result), 0,
                 info = "Nonexistent region should return empty data frame")
  }

  # Nonexistent city with search pattern - should return empty results
  result <- wkls$us$ca[["%nonexistentcity%"]]
  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 0,
               info = "Nonexistent city search should return empty data frame")
})

test_that("geometry methods fail gracefully on empty results", {
  skip_if_offline()

  # Create a chain that will return empty results
  empty_chain <- wkls$us$ca$nonexistentcity

  expect_error(
    empty_chain$wkt(),
    "No result found for: us\\.ca\\.nonexistentcity",
    info = "Should raise error for nonexistent location"
  )
})

test_that("chained objects properly propagate errors", {
  skip_if_offline()

  # Get a valid proxy first
  us_proxy <- wkls$us

  # countries() should fail on chained data
  expect_error(
    us_proxy$countries(),
    "countries\\(\\) can only be called on the root object",
    info = "countries() should not work on chained proxy"
  )

  # regions() should fail on chained data (more than 1 level)
  ca_proxy <- wkls$us$ca
  expect_error(
    ca_proxy$regions(),
    "regions\\(\\) requires exactly one level of chaining",
    info = "regions() should not work with 2+ levels of chaining"
  )
})

test_that("overture_version() blocked on chained objects", {
  skip_if_offline()

  # Should work at root
  expect_type(wkls$overture_version(), "character")

  # Should fail on chained objects
  expect_error(
    wkls$us$overture_version(),
    "overture_version\\(\\) is only available at the root level",
    info = "overture_version() should only work at root level"
  )
})
