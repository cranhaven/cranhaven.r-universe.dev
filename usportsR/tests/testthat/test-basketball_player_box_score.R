test_that("basketball_player_box_score() works", {
  womens_2013 <- basketball_player_box_score("w",2013)
  expect_type(womens_2013,"list")
  expect_true(nrow(womens_2013) != 0)
})

test_that("Multiple years that include some that don't exist",{
  mens_box_score <- basketball_player_box_score("m",c(2010,2050))
  expect_type(mens_box_score,"list")
  expect_true(nrow(mens_box_score) != 0)
})
