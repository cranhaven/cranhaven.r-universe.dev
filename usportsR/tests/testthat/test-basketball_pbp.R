test_that("basketball_pbp() mens works", {
  mens_2019_pbp <- basketball_pbp("m",2019)
  expect_type(mens_2019_pbp,"list")
  expect_true(nrow(mens_2019_pbp) != 0)
})

test_that("if a season isn't available it works", {
  womens_1000_pbp <- basketball_pbp("w",1000)
  expect_type(womens_1000_pbp,"list")
  expect_true(nrow(womens_1000_pbp)==0)
})
