test_that("football_pbp() test", {
  pbp_2021 <- football_pbp(2021)
  expect_type(pbp_2021,"list")
  expect_true(nrow(pbp_2021) != 0)
})
