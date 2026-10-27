test_that("football_scoring_summaries() test", {
  scoring_summaries_2022 <- football_scoring_summaries(2022)
  expect_type(scoring_summaries_2022,"list")
  expect_true(nrow(scoring_summaries_2022) != 0)
})
