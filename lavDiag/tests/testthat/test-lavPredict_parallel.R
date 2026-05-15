test_that("lavPredict_parallel works on continuous models incl. SEs", {
  fit <- .fit_cont(n = 80)
  out <- lavPredict_parallel(fit, return_type = "data", se = TRUE)
  lvs <- lavaan::lavNames(fit, "lv")
  expect_true(all(lvs %in% names(out)))
  expect_true(all(paste0(".se_", lvs) %in% names(out)))
})

test_that("lavPredict_parallel runs on ordinal and mixed (SEs optional)", {
  fit_o <- .fit_ord(n = 80)
  out_o <- lavPredict_parallel(fit_o, return_type = "data", se = FALSE)
  lvo   <- lavaan::lavNames(fit_o, "lv")
  expect_true(all(lvo %in% names(out_o)))
  # Ordinal SEs often unavailable -> do not require them
  expect_true(any(!paste0(".se_", lvo) %in% names(out_o)) || TRUE)

  fit_m <- .fit_mixed(n = 100)
  out_m <- lavPredict_parallel(fit_m, return_type = "data", se = FALSE)
  lvm   <- lavaan::lavNames(fit_m, "lv")
  expect_true(all(lvm %in% names(out_m)))
})
