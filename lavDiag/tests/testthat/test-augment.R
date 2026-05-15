test_that("augment returns anchors, predictions and residuals (continuous)", {
  fit <- .fit_cont(n = 60)
  aug <- augment(fit)
  expect_s3_class(aug, "data.frame")
  expect_true(all(c(".rid",".gid",".group") %in% names(aug)))
  ov <- intersect(lavaan::lavNames(fit, "ov"), names(aug))[1]
  expect_true(paste0(".yhat_",  ov) %in% names(aug))
  expect_true(paste0(".resid_", ov) %in% names(aug))
})

test_that("augment works on mixed model (yhat present at least)", {
  fit <- .fit_mixed(n = 100)
  aug <- augment(fit)
  ov  <- intersect(lavaan::lavNames(fit, "ov"), names(aug))[1]
  expect_true(paste0(".yhat_", ov) %in% names(aug))
})
