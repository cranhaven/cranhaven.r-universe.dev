
test_that("input is validated", {

  expect_error(
    mts_getDistance()
  )

  expect_error(
    mts_getDistance(
      example_mts,
      longitude = -122.234,
      latitude = "bop"
    )
  )

  expect_error(
    mts_getDistance(
      example_mts,
      longitude = -122.234,
      latitude = 120.0
    )
  )

  expect_error(
    mts_getDistance(
      example_mts,
      longitude = -122.234,
      latitude = 47.234,
      measure = "bop"
    )
  )

})

test_that("simple distance works", {

  # Garfield Medical Center in LA
  longitude <- -118.12321
  latitude <- 34.06775

  distances <- mts_getDistance(
    mts = example_mts,
    longitude = longitude,
    latitude = latitude
  )

  expect_identical(
    length(distances),
    nrow(example_mts$meta)
  )

  expect_identical(
    names(distances),
    example_mts$meta$deviceDeploymentID
  )

  expect_equal(
    as.numeric(distances[distances <= 1000]),
    c(761.2503, 844.0852, 743.9420, 765.9372, 950.1779),
    tolerance = 0.1
  )


})

