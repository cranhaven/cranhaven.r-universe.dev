test_that("testing field_hockey_pbp()", {
  fh_2019 <- field_hockey_pbp(2019)
  expect_type(fh_2019,"list")
  expect_true(nrow(fh_2019) != 0)
})
