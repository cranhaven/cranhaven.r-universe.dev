test_that("soccer_team_box_score() test", {
  team_2018 <- soccer_team_box_score("w",2018)
  expect_type(team_2018,"list")
  expect_true(nrow(team_2018) != 0)
})
