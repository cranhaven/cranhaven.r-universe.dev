test_that("mts_select() works", {

  # Camp Fire dataset has 134 timeseries
  ids <- Camp_Fire$meta$deviceDeploymentID

  # Subsetting
  expect_equal(
    mts_select(Camp_Fire, ids[1:2]) %>% mts_extractMeta() %>% nrow(),
    2
  )

  expect_equal(
    mts_select(Camp_Fire, ids[11:18]) %>% mts_extractMeta() %>% nrow(),
    8
  )

  # Return a valid mts object when nothing matches
  expect_equal(
    mts_select(Camp_Fire, "Rumplestiltskin") %>% mts_isValid(),
    TRUE
  )

  expect_equal(
    mts_select(Camp_Fire, "Rumplestiltskin") %>% mts_isEmpty(),
    TRUE
  )

  expect_equal(
    mts_select(Camp_Fire, "Rumplestiltskin") %>% mts_extractMeta() %>% nrow(),
    0
  )

  # Reordering
  expect_equal(
    mts_select(Camp_Fire, rev(ids)) %>% mts_extractMeta() %>% dplyr::pull("deviceDeploymentID"),
    rev(ids)
  )

})
