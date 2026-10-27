test_that("tnf_athlete_rankings() test", {
  athletes <- tnf_athlete_rankings()
  expect_type(athletes, "list")
  expect_true(nrow(athletes) != 0)
})
