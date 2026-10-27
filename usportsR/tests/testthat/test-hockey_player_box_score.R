test_that("hockey_player_box_score() test", {
  player_box_score_2015 <- hockey_player_box_score("m",2015)
  expect_type(player_box_score_2015,"list")
  expect_true(nrow(player_box_score_2015) != 0)
})
