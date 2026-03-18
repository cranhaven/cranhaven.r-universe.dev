set.seed(44)

# model to use in tests ---------------------------------------------------

data(lalonde)
confounders <- c('age', 'educ', 'black', 'hisp', 'married', 'nodegr')
model_results <- bartCause::bartc(
  response = lalonde[['re78']],
  treatment = lalonde[['treat']],
  confounders = as.matrix(lalonde[, confounders]),
  estimand = 'ate',
  commonSup.rule = 'none',
  keepTrees = TRUE,
  seed = 44
)


# plots to test -----------------------------------------------------------

out_balance <- plot_balance(.data = lalonde, treatment = 'treat', confounders = confounders)
out_support_none <- plot_common_support(.model = model_results, rule = 'both')
out_support_sd <- plot_common_support(.model = model_results, rule = 'sd')
out_support_chi <- plot_common_support(.model = model_results, rule = 'chi')
# out_ITE <- plot_ITE(.model = model_results)
out_overlap_pscores_hist <- plot_overlap_pScores(
  .data = lalonde,
  treatment = 'treat',
  response = 're78',
  confounders = confounders,
  plot_type = 'histogram',
  seed = 44
)
out_overlap_pscores_density <- plot_overlap_pScores(
  .data = lalonde,
  treatment = 'treat',
  response = 're78',
  confounders = confounders,
  plot_type = 'density',
  seed = 44
)
out_overlap_vars_hist <- plot_overlap_vars(
  .data = lalonde,
  treatment = 'treat',
  confounders = confounders,
  plot_type = 'histogram'
)
out_overlap_vars_density <- plot_overlap_vars(
  .data = lalonde,
  treatment = 'treat',
  confounders = confounders,
  plot_type = 'density'
)
out_trace <- plot_trace(.model = model_results)
out_CATE <- plot_CATE(
  model_results,
  type = 'density',
  ci_80 = TRUE,
  ci_95 = TRUE,
  reference = 0,
  .mean = TRUE,
  .median = TRUE
)
plot_ICATE <- plot_ICATE(model_results, .group_by = NULL, n_bins = 30, .alpha = .7)
out_PATE <- plot_PATE(
  model_results,
  type = 'density',
  ci_80 = TRUE,
  ci_95 = TRUE,
  reference = 0,
  .mean = TRUE,
  .median = TRUE
)
out_SATE <- plot_SATE(
  model_results,
  type = 'density',
  ci_80 = TRUE,
  ci_95 = TRUE,
  reference = 0,
  .mean = TRUE,
  .median = TRUE
)
out_waterfall <- plot_waterfall(
  model_results,
  descending = FALSE,
  .order = NULL,
  .color = NULL,
  .alpha = 0.5
)
out_waterfall_2 <- plot_waterfall(
  model_results,
  descending = TRUE,
  .order = lalonde$age,
  .color = lalonde$educ
)
out_moderator_c_pd <- plot_moderator_c_pd(
  model_results,
  moderator = lalonde$educ)
out_moderator_c_loess <- plot_moderator_c_loess(
  model_results,
  moderator = lalonde$educ,
  line_color = 'blue')
out_moderator_d_density <- plot_moderator_d_density(
  model_results,
  moderator = lalonde$educ,
  .alpha = 0.7,
  facet = FALSE,
  .ncol = 1)
out_moderator_d_linerange <- plot_moderator_d_linerange(
  model_results,
  moderator = lalonde$educ,
  .alpha = 0.7,
  horizontal = FALSE)
out_moderator_search <- plot_moderator_search(
  model_results,
  max_depth = 2
)


# tests -------------------------------------------------------------------

test_that("plot_balance() output is correct", {
  expect_s3_class(out_balance, 'ggplot')
  vdiffr::expect_doppelganger('balance', out_balance)
})
test_that("plot_common_support() output is correct", {
  expect_s3_class(out_support_none, 'ggplot')
  vdiffr::expect_doppelganger('supportNone', out_support_none)
  expect_s3_class(out_support_sd, 'ggplot')
  vdiffr::expect_doppelganger('supportSD', out_support_sd)
  expect_s3_class(out_support_chi, 'ggplot')
  vdiffr::expect_doppelganger('supportChi', out_support_chi)
})
test_that("plot_overlap_pScores() output is correct", {
  expect_s3_class(out_overlap_pscores_hist, 'ggplot')
  vdiffr::expect_doppelganger('overlapPscoresHist', out_overlap_pscores_hist)
  expect_s3_class(out_overlap_pscores_density, 'ggplot')
  vdiffr::expect_doppelganger('overlapPscoresDensity', out_overlap_pscores_density)
})
test_that("plot_overlap_vars() output is correct", {
  expect_s3_class(out_overlap_vars_hist, 'ggplot')
  vdiffr::expect_doppelganger('overlapVarsHist', out_overlap_vars_hist)
  expect_s3_class(out_overlap_vars_density, 'ggplot')
  vdiffr::expect_doppelganger('overlapVarsDensity', out_overlap_vars_density)
})
test_that("plot_trace() output is correct", {
  expect_s3_class(out_trace, 'ggplot')
  vdiffr::expect_doppelganger('trace', out_trace)
})
test_that("plot_*ATE outputs are all correct", {
  expect_s3_class(out_CATE, 'ggplot')
  vdiffr::expect_doppelganger('CATE', out_CATE)
  expect_s3_class(out_PATE, 'ggplot')
  vdiffr::expect_doppelganger('PATE', out_PATE)
  expect_s3_class(out_SATE, 'ggplot')
  vdiffr::expect_doppelganger('SATE', out_SATE)
})
test_that("plot_waterfall() output is correct", {
  expect_s3_class(out_waterfall, 'ggplot')
  vdiffr::expect_doppelganger('waterfall1', out_waterfall)
  expect_s3_class(out_waterfall_2, 'ggplot')
  vdiffr::expect_doppelganger('waterfall2', out_waterfall_2)
})
test_that("plot_moderator_* outputs are all corrects", {
  expect_s3_class(out_moderator_c_pd, 'ggplot')
  vdiffr::expect_doppelganger('mod_c_pd', out_moderator_c_pd)
  expect_s3_class(out_moderator_c_loess, 'ggplot')
  vdiffr::expect_doppelganger('mod_c_loess', out_moderator_c_loess)
  expect_s3_class(out_moderator_d_density, 'ggplot')
  vdiffr::expect_doppelganger('mod_d_density', out_moderator_d_density)
  expect_s3_class(out_moderator_d_linerange, 'ggplot')
  vdiffr::expect_doppelganger('mod_d_linerange', out_moderator_d_linerange)
  expect_s3_class(out_moderator_search, 'ggplot')
  vdiffr::expect_doppelganger('mod_search', out_moderator_search)
})
