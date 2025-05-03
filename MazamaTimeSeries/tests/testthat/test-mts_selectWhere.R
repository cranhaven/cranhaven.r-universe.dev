test_that("mts_selectWhere() works", {

  sites_500 <-
    Camp_Fire %>%
    mts_selectWhere(
      function(x) { any(x >= 500, na.rm = TRUE) }
    )

  # 12 sites have values >= 500
  expect_equal(
    nrow(sites_500$meta),
    12
  )

  # Return a valid mts object when nothing matches
  sites_5000 <-
    Camp_Fire %>%
    mts_selectWhere(
      function(x) { any(x >= 5000, na.rm = TRUE) }
    )

  expect_equal(
    mts_isValid(sites_5000),
    TRUE
  )

  expect_equal(
    mts_isEmpty(sites_5000),
    TRUE
  )

  expect_equal(
    nrow(sites_5000$meta),
    0
  )

})

test_that("mts_selectWhere() generates errors where appropriate", {

  # Missing mts
  expect_error(
    mts_selectWhere(
      function(x) { any(x >= 5000, na.rm = TRUE) }
    )
  )

  # Invalid mts
  expect_error(
    mts_selectWhere(
      123,
      function(x) { any(x >= 5000, na.rm = TRUE) }
    )
  )

  # mts not found
  expect_error(
    mts_selectWhere(
      Rumplestiltskin,
      function(x) { any(x >= 5000, na.rm = TRUE) }
    )
  )

  # mts has no data
  expect_error(
    mts_selectWhere(
      monitor_select(Camp_Fire, "Rumplestiltskin"),
      function(x) { any(x >= 5000, na.rm = TRUE) }
    )
  )

  # FUN is not a function
  expect_error(
    mts_selectWhere(
      Camp_Fire,
      123
    )
  )

  # FUN is missing na.rm = TRUE
  expect_error(
    mts_selectWhere(
      Camp_Fire,
      function(x) { any(x >= 5000, na.rm = FALSE) }
    )
  )

})
