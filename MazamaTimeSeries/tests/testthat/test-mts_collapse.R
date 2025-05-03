
test_that("input is validated", {

  expect_error(
    mts_collapse()
  )

  expect_error(
    mts_collapse(example_mts, FUN = NULL)
  )

  expect_error(
    mts_collapse(
      example_mts,
      longitude = -122.234,
      latitude = "bop"
    )
  )

  expect_error(
    mts_collapse(
      example_mts,
      longitude = -122.234,
      latitude = 120.0
    )
  )

  expect_error(
    mts_collapse(
      example_mts,
      longitude = -122.234,
      latitude = 47.234,
      FUN = NULL
    )
  )

  expect_error(
    mts_collapse(
      example_mts,
      longitude = -122.234,
      latitude = 47.234,
      FUN = "bop"
    )
  )

})

test_that("simple collapse works", {

  mon1 <- mts_collapse(example_mts)

  expect_true(mts_isValid(mon1))

  expect_identical(
    mon1$meta$longitude,
    mean(example_mts$meta$longitude)
  )

  expect_identical(
    mon1$meta$latitude,
    mean(example_mts$meta$latitude)
  )

  expect_identical(
    mon1$meta$deviceID,
    "generatedID"
  )

  expect_identical(
    mon1$meta$timezone,
    "America/Los_Angeles"
  )

})

test_that("advanced collapse works", {

  # Garfield Medical Center in LA
  longitude <- -118.12321
  latitude <- 34.06775

  mon2 <- mts_collapse(
    example_mts,
    longitude = longitude,
    latitude = latitude,
    deviceID = "Special ID"
  )

  expect_true(mts_isValid(mon2))

  expect_identical(
    mon2$meta$longitude,
    longitude
  )

  expect_identical(
    mon2$meta$latitude,
    latitude
  )

  expect_identical(
    mon2$meta$deviceID,
    "Special ID"
  )

})

