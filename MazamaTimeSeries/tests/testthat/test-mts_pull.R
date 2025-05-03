test_that("input is validated", {

  expect_error(
    mts_pull()
  )

  expect_error(
    mts_pull(example_mts, var = NULL)
  )

  expect_error(
    mts_pull(
      example_mts,
      var = "RumpleStiltskin"
    )
  )

})

test_that("data and meta can be pulled", {

  expect_true(
    example_mts %>%
      mts_pull("da4cadd2d6ea5302_4686") %>%
      is.numeric()
  )

  expect_true(
    example_mts %>%
      mts_pull("communityRegion") %>%
      is.character()
  )

})
