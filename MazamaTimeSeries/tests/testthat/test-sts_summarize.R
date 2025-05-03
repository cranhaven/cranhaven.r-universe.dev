
test_that("input is validated", {

  bop <- example_raws

  expect_error(
    sts_summarize()
  )

  expect_error(
    sts_summarize(bop, FUN = NULL)
  )

  expect_error(
    sts_summarize(
      bop,
      unit = "furlong",
      FUN = mean
    )
  )

})

test_that("simple daily summary works", {

  bop <- example_raws

  # Local time days
  daily <- sts_summarize(
    bop,
    timezone = NULL,
    unit = "day",
    FUN = mean,
    na.rm = TRUE,
    minCount = 18
  )

  # proper values
  expect_identical(
    dplyr::pull(bop$data, 2)[1:24] %>% mean(na.rm = TRUE),
    daily$data[1,2] %>% as.numeric()
  )

})

test_that("na.rm and minCount work", {

  bop <- example_raws
  bop$data[1:6,2] <- NA

  # na.rm = FALSE
  daily <- sts_summarize(
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
  daily <- sts_summarize(
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
  daily <- sts_summarize(
    bop,
    timezone = NULL,
    unit = "day",
    FUN = mean,
    na.rm = TRUE,
    minCount = 18
  )

  # proper values
  expect_identical(
    dplyr::pull(bop$data, 2)[1:24] %>% mean(na.rm = TRUE),
    daily$data[1,2] %>% as.numeric()
  )

})

