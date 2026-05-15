test_that("resid_corrplot records a plot object", {
  fit <- .fit_mixed(n = 100)
  rec <- resid_corrplot(fit, type = "cor.bentler", order = "hclust", record = TRUE)
  expect_true(inherits(rec, "recordedplot"))
})

test_that("resid_corrplot handles multi-group with common_scale", {
  fit <- .fit_mixed_mg(n = 200)
  rec <- resid_corrplot(fit, type = "cor.bentler", common_scale = TRUE, record = TRUE)
  expect_true(inherits(rec, "recordedplot") || is.list(rec))
})
