test_that("model_info handles continuous, ordinal, and mixed models", {
  mi_cont  <- model_info(.fit_cont(n = 80))
  mi_ord   <- model_info(.fit_ord(n = 100))
  mi_mixed <- model_info(.fit_mixed(n = 100))

  expect_true(length(mi_cont$latent_variables)   >= 1)
  expect_true(length(mi_mixed$ov_ordinal)        >= 3)
  expect_true(length(mi_mixed$ov_continuous)     >= 3)
  expect_true(mi_ord$parameterization %in% c("delta","theta", NA_character_))
})
