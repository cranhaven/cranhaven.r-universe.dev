test_that("swimming_athlete_rankings", {
  rankings <- swimming_athlete_rankings()
  expect_type(rankings,"list")
  expect_true(nrow(rankings) != 0)
})
