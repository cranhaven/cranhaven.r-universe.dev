test_that("fh player box score", {
  fh_box_score <- field_hockey_player_box_score(2022)
  expect_type(fh_box_score,"list")
  expect_true(nrow(fh_box_score)!= 0)
})
