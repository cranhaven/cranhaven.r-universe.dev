
test_that("input is validated", {

  expect_error(
    mts_summarize()
  )

  expect_error(
    mts_summarize(example_mts, FUN = NULL)
  )

  expect_error(
    mts_summarize(
      example_mts,
      unit = "furlong",
      FUN = mean
    )
  )

})

test_that("simple daily summary works", {

  # Local time days
  daily <- mts_summarize(
    Carmel_Valley,
    timezone = NULL,
    unit = "day",
    FUN = mean,
    na.rm = TRUE,
    minCount = 18
  )

  # Proper time axis
  PDT_days <- seq(
    MazamaCoreUtils::parseDatetime("2016-07-22", timezone = "America/Los_Angeles"),
    MazamaCoreUtils::parseDatetime("2016-08-14", timezone = "America/Los_Angeles"),
    by = "day"
  )

  # proper values
  expect_identical(
    mean(Carmel_Valley$data[1:24,2], na.rm = TRUE),
    daily$data[1,2] %>% as.numeric()
  )

})

test_that("na.rm and minCount work", {

  # The first day of Carmel_Valley data already has one NA
  #
  # > Carmel_Valley$data[1:24,2]
  # [1]  0  0  0  1  2  2  1  0  0  1  2  3  3  3  4  5  3  0  1  5  7 NA  4  0

  # Add 5 more to be on the threshold for minCount = 18
  bop <- Carmel_Valley
  bop$data[1:5,2] <- NA

  # na.rm = FALSE
  daily <- mts_summarize(
    bop,
    timezone = NULL,
    unit = "day",
    FUN = mean,
    na.rm = FALSE,
    minCount = 18
  )

  expect_identical(
    as.logical(NA),
    daily$data[1,2] %>% as.logical()
  )

  # minCount > valid records
  daily <- mts_summarize(
    bop,
    timezone = NULL,
    unit = "day",
    FUN = mean,
    na.rm = TRUE,
    minCount = 19
  )

  expect_identical(
    as.logical(NA),
    daily$data[1,2] %>% as.logical()
  )

  # proper usage
  daily <- mts_summarize(
    bop,
    timezone = NULL,
    unit = "day",
    FUN = mean,
    na.rm = TRUE,
    minCount = 18
  )

  # proper values
  expect_identical(
    mean(bop$data[1:24,2], na.rm = TRUE),
    daily$data[1,2] %>% as.numeric()
  )

})

