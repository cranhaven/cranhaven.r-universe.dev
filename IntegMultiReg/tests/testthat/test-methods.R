test_that("print returns its argument invisibly and prints key fields", {
  out <- capture.output(res <- print(fit_bin))
  expect_identical(res, fit_bin)
  expect_true(any(grepl("Outcome type", out)))
  expect_true(any(grepl("Platform key", out)))
  expect_true(any(grepl("P1 = genomic", out, fixed = TRUE)))
  expect_true(any(grepl("Availability subgroups modelled", out)))
  expect_true(any(grepl("011 : P1 + P2", out, fixed = TRUE)))
})

test_that("summary produces a per-platform selection table", {
  s <- summary(fit_bin, threshold = 0.5)
  expect_s3_class(s, "summary.imr")
  expect_named(s$selected, c("genomic", "proteomic", "metabolomic"))
  for (df in s$selected) {
    expect_true(all(c("feature", "max_mpip", "subgroup") %in% names(df)))
    # Selection uses the unrounded max mPIP, but the reported max_mpip is
    # rounded to 3 dp, so a feature just above the threshold displays as 0.500.
    if (nrow(df) > 0) expect_true(all(df$max_mpip >= 0.5))
  }
  expect_output(print(s), "summary")
})

test_that("snake-case summary and coef wrappers dispatch through S3", {
  expect_equal(summary_imr(fit_bin, threshold = 0.5),
               summary(fit_bin, threshold = 0.5))
  expect_equal(coef_imr(fit_bin), coef(fit_bin))
})

test_that("coef returns named per-platform mPIP matrices", {
  mp <- coef(fit_bin)
  expect_type(mp, "list")
  expect_named(mp, c("genomic", "proteomic", "metabolomic"))
  expect_equal(rownames(mp$genomic),
               fit_bin$model_bitstrings[fit_bin$platform_models[[1]]])
})

test_that("plot methods run without error for each type", {
  tmp <- tempfile(fileext = ".pdf")
  pdf(tmp)
  on.exit({ dev.off(); unlink(tmp) }, add = TRUE)
  expect_silent(plot(fit_bin, type = "selection"))
  expect_silent(plot(fit_bin, type = "theta"))
  expect_silent(plot(fit_bin, type = "trace"))
  expect_silent(plot(fit_bin, type = "selection", platform = 1))
  expect_silent(plot_imr(fit_bin, type = "selection", platform = 1))
})
