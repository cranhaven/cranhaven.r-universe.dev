test_that("soccer_pbp() test", {
  pbp_2019 <- soccer_pbp("w",2019)
  expect_type(pbp_2019,"list")
  expect_true(nrow(pbp_2019) != 0)
})
