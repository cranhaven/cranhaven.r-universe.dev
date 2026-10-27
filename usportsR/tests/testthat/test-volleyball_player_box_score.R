test_that("volleyball_player_box_score() test", {
  players_2015 <- volleyball_player_box_score("w",2015)
  expect_type(players_2015,"list")
  expect_true(nrow(players_2015) != 0)
})
