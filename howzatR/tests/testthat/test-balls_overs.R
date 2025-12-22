
# balls_to_overs ----------------------------------------------------------

test_that("balls_to_overs work", {
  expect_equal(balls_to_overs(balls = 10), 1.4)
  expect_equal(balls_to_overs(balls = 10 * 6), 10)
})


# overs_to_balls ----------------------------------------------------------

test_that("overs_to_balls work", {
  expect_equal(overs_to_balls(overs = 10.2), 62)
  expect_equal(overs_to_balls(overs = 10 * 6), 60 * 6)
})

test_that("overs_to_balls greater than 6 Balls supplied causes error", {
  balls_no <- 8

  expect_error(overs_to_balls(overs = 10.8),
    regexp = paste(
      "Oops... an over can only contain 6 legitimate balls. You supplied",
      balls_no, "balls which is impossible. Please check your input!"
    )
  )
})
