test_that("swimming_team_rankings() test", {
  team <- swimming_team_rankings()
  expect_type(team , "list")
  expect_true(nrow(team) != 0)
})
