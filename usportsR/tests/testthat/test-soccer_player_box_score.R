test_that("soccer_player_box_score", {
  player_2022 <- soccer_player_box_score("w", 2022)
  expect_type(player_2022,"list")
  expect_true(nrow(player_2022) != 0)
})
