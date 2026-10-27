test_that("tnf_team_rankings() test", {
  team <- tnf_team_rankings()
  expect_type(team , "list")
  expect_true(nrow(team) != 0)
})
