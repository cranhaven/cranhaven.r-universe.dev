data(lalonde)
pscores <- propensity_scores(
  .data = lalonde,
  treatment = 'treat',
  response = 're78',
  confounders = c('age', 'educ')
)


test_that("propensity_scores() output is correct", {
  expect_equal(length(pscores), nrow(lalonde))
  expect_vector(pscores)
})
