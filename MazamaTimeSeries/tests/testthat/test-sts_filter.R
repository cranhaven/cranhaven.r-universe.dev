test_that("basic filtering", {

  expect_equal(
    sts_filter(example_sts, temperature > 90) %>% sts_extractData() %>% nrow(),
    342
  )

  expect_equal(
    sts_filter(example_sts, temperature > 90, humidity < 20) %>% sts_extractData() %>% nrow(),
    126
  )

  expect_equal(
    sts_filter(example_sts, temperature > 900) %>% sts_isEmpty(),
    TRUE
  )

  # Filtering by the wrong type (numeric column by character) should return an empty sts
  expect_equal(
    sts_filter(example_sts, temperature > "string") %>% sts_isEmpty(),
    TRUE
  )

  # Filtering by a column that doesn't exist should give an error
  expect_error(
    sts_filter(example_sts, doesntexist > 100)
  )

  # Multiple filtering steps on an empty sts should not generate an error
  expect_equal(
    example_sts %>%
      sts_filter(temperature > 900) %>%
      sts_filter(temperature < 100) %>%
      sts_filter(temperature > 900) %>%
      sts_isEmpty(),
    TRUE
  )


})
