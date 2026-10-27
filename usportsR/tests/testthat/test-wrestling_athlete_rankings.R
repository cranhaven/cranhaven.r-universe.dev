test_that("wrestling_athlete_rankings() test", {
  athletes <- wrestling_athlete_rankings("m" , 100)
  expect_type(athletes,"list")
  expect_true(nrow(athletes)  != 0)
})
