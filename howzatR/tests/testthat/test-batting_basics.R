
# bat_avg -----------------------------------------------------------

test_that("bat_avg works", {
  total_runs <- sum(c(45, 123, 56, 12, 192, 34, 78, 3, 25))

  expect(bat_avg(runs_scored = 50, no_dismissals = 2), 25)

  expect_equal(
    bat_avg(runs_scored = 568, no_dismissals = 9),
    bat_avg(runs_scored = total_runs, no_dismissals = 9)
  )
})


# bat_sr ------------------------------------------------------------

test_that("bat_sr works", {
  total_runs <- sum(c(45, 123, 56, 12, 192, 34, 78, 3, 25))
  total_balls <- sum(c(50, 120, 78, 3, 226, 36, 45, 12, 30))

  expect(bat_sr(runs_scored = 250, balls_faced = 200), 125)

  expect_equal(
    bat_sr(runs_scored = 568, balls_faced = 600),
    bat_sr(
      runs_scored = total_runs,
      balls_faced = total_balls
    )
  )
})
