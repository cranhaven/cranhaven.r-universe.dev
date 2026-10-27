test_that("xc_team_rankings() test", {
  team <- xc_team_rankings()
  expect_type(team,"list")
  expect_true(nrow(team) != 0)
})
