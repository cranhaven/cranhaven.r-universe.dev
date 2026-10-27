test_that("hockey_team_box_score() test", {
  team_2023 <- hockey_team_box_score("w",2023)
  expect_type(team_2023,"list")
  expect_true(nrow(team_2023) != 0)
})
