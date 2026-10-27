test_that("fh team box score", {
  team_box_score_2023 <- field_hockey_team_box_score(2023)
  expect_type(team_box_score_2023,"list")
  expect_true(nrow(team_box_score_2023) != 0)
})
