test_that("mts_filterData() works", {

  expect_equal(
    mts_filterData(example_mts, da4cadd2d6ea5302_4686 > 150) %>% mts_extractData() %>% nrow(),
    2
  )

  expect_equal(
    mts_filterData(example_mts, da4cadd2d6ea5302_4686 > 900) %>% mts_isEmpty(),
    TRUE
  )

  # Filtering by the wrong type (numeric column by character) should return an empty mts
  expect_equal(
    mts_filterData(example_mts, da4cadd2d6ea5302_4686 > "string") %>% mts_isEmpty(),
    TRUE
  )

  # Filtering by a column that doesn't exist should give an error
  expect_error(
    mts_filterData(example_mts, doesntexist > 100)
  )

})
