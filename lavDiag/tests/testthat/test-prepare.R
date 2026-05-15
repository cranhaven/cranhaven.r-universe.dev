test_that("prepare builds latent grids and m_est_* curves (continuous)", {
  fit <- .fit_cont(n = 60)
  prp <- prepare(fit, length.out = 21)
  expect_s3_class(prp, "data.frame")
  expect_true(all(c(".rid",".gid",".group",".latent_var") %in% names(prp)))
  ov <- lavaan::lavNames(fit, "ov")[1]
  expect_true(any(grepl(paste0("^m_est_", ov, "_"), names(prp))))
})

test_that("prepare merges branches for mixed model", {
  fit <- .fit_mixed(n = 100)
  prp <- prepare(fit, length.out = 15)
  expect_true(all(c(".rid",".gid",".group",".latent_var") %in% names(prp)))
})
