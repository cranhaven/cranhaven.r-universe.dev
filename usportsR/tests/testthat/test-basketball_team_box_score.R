test_that("basketball_team_box_score()", {
  team_box_score_2011 <- basketball_team_box_score("w",2011)
  expect_type(team_box_score_2011,"list")
  expect_true(nrow(team_box_score_2011)!= 0)
})
