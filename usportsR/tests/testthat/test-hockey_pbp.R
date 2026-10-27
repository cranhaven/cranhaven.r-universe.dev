test_that("hockey_pbp() test", {
  pbp_2013 <- hockey_pbp("m", 2013)
  expect_type(pbp_2013,"list")
  expect_true(nrow(pbp_2013) != 0)
})
