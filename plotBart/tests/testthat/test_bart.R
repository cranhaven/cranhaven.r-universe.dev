# goal is catch any big changes to the bartCause api

data(lalonde)
confounders <- c('age', 'educ', 'black', 'hisp', 'nodegr')
model_results <- bartCause::bartc(
  response = lalonde[['re78']],
  treatment = lalonde[['treat']],
  confounders = as.matrix(lalonde[, confounders]),
  estimand = 'ate',
  group.by = lalonde[['married']],
  group.effects = TRUE,
  commonSup.rule = 'sd',
  keepTrees = TRUE,
  seed = 2
)

out <- validate_model_(model_results)
slots <- attributes(model_results)

slots_original <- list(
  names = c(
    'fit.rsp',
    'data.rsp',
    'fit.trt',
    'mu.hat.obs',
    'mu.hat.cf',
    'p.score',
    'samples.p.score',
    'method.rsp',
    'method.trt',
    'estimand',
    'commonSup.rule',
    'commonSup.cut',
    'name.trt',
    'trt',
    'sd.obs',
    'sd.cf',
    'commonSup.sub',
    'missingRows',
    'est',
    'fitPars',
    'call',
    'group.by',
    'use.ranef',
    'group.effects',
    'n.chains',
    'seed'
  ),
  class = c('bartcFit')
)

test_that("bartCause::bartc() API still works", {
  expect_s3_class(model_results, 'bartcFit')
  expect_identical(slots, slots_original)
})

test_that('validate_model_() works', {
  expect_null(out)
})
