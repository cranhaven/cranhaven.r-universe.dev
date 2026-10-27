test_that("wrestling_team_rankings() test", {
  team <- wrestling_team_rankings("m")
  expect_type(team, "list")
  expect_true(nrow(team) != 0)
})

