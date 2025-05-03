test_that("basic metadata filtering", {

  expect_equal(
    mts_filterMeta(example_mts, communityRegion == "El Monte") %>% mts_extractMeta() %>% nrow(),
    5
  )

  expect_equal(
    mts_filterMeta(example_mts, communityRegion == "Temescal Valley") %>% mts_isEmpty(),
    TRUE
  )

  # Filtering by the wrong type (numeric column by character) should return an empty mts
  expect_equal(
    mts_filterMeta(example_mts, longitude > "string") %>% mts_isEmpty(),
    TRUE
  )

  # Filtering by a column that doesn't exist should give an error
  expect_error(
    mts_filterMeta(example_mts, doesntexist > 100)
  )

  # Multiple filtering steps on an empty mts should not generate an error
  expect_equal(
    example_mts %>%
      mts_filterMeta(communityRegion == "Temescal Valley") %>%
      mts_filterMeta(stateCode == "IA") %>%
      mts_filterMeta(stateCode == "NY") %>%
      mts_filterMeta(stateCode == "CA") %>%
      mts_isEmpty(),
    TRUE
  )

})
