test_that("correctly names columns", {
  S <- make_synthetic_data(1, 3)
  x <- names(S)
  expect_equal(x, c("group", "item_1", "item_2", "item_3"))
})

test_that("outputs correct number of columns and rows", {
  S <- make_synthetic_data(1, 3)
  c <- length(S)
  r <- length(S$group)
  expect_equal(c(c, r), c(4, 1))
})
