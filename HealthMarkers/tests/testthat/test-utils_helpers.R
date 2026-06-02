## Tests for .hm_build_col_map() internal helper
## Relies on HealthMarkers::: to access unexported functions.

test_that(".hm_build_col_map: NULL col_map infers all resolvable keys", {
  skip_on_cran()
  df <- tibble::tibble(hdlc = 1, trig = 1.3, bmi = 24)
  res <- HealthMarkers:::.hm_build_col_map(df, NULL, c("HDL_c", "TG", "BMI", "glucose"), fn = "test")
  # HDL_c, TG, BMI should be inferred from synonym dictionary
  expect_equal(res$col_map[["HDL_c"]], "hdlc")
  expect_equal(res$col_map[["TG"]],    "trig")
  expect_equal(res$col_map[["BMI"]],   "bmi")
  # glucose not in data -> not resolved
  expect_null(res$col_map[["glucose"]])
  # all three resolved keys are inferred (none user-supplied)
  expect_setequal(res$inferred_keys, c("HDL_c", "TG", "BMI"))
  expect_length(res$user_keys, 0L)
})

test_that(".hm_build_col_map: partial col_map — user entries preserved, rest inferred", {
  skip_on_cran()
  df  <- tibble::tibble(pglu0 = 5.5, hdlc = 1, trig = 1.3, bmi = 24)
  cm  <- list(G0 = "pglu0")  # user supplies only G0
  res <- HealthMarkers:::.hm_build_col_map(df, cm, c("G0", "HDL_c", "TG", "BMI"), fn = "test")
  # user entry preserved
  expect_equal(res$col_map[["G0"]], "pglu0")
  # remaining keys inferred from dictionary
  expect_equal(res$col_map[["HDL_c"]], "hdlc")
  expect_equal(res$col_map[["TG"]],    "trig")
  expect_equal(res$col_map[["BMI"]],   "bmi")
  # split is correct
  expect_equal(res$user_keys, "G0")
  expect_setequal(res$inferred_keys, c("HDL_c", "TG", "BMI"))
})

test_that(".hm_build_col_map: full col_map — nothing inferred, nothing overwritten", {
  skip_on_cran()
  df  <- tibble::tibble(pglu0 = 5.5, hdlc = 1, trig = 1.3, bmi = 24)
  cm  <- list(G0 = "pglu0", HDL_c = "hdlc", TG = "trig", BMI = "bmi")
  res <- HealthMarkers:::.hm_build_col_map(df, cm, c("G0", "HDL_c", "TG", "BMI"), fn = "test")
  expect_equal(res$col_map[["G0"]],    "pglu0")
  expect_equal(res$col_map[["HDL_c"]], "hdlc")
  expect_equal(res$col_map[["TG"]],    "trig")
  expect_equal(res$col_map[["BMI"]],   "bmi")
  expect_setequal(res$user_keys, c("G0", "HDL_c", "TG", "BMI"))
  expect_length(res$inferred_keys, 0L)
})

test_that(".hm_build_col_map: alias materialized into data", {
  skip_on_cran()
  # col_map maps G0 -> "pglu0"; after call data[["G0"]] should exist
  df  <- tibble::tibble(pglu0 = c(5.5, 6.0), hdlc = c(1, 1.2))
  cm  <- list(G0 = "pglu0")
  res <- HealthMarkers:::.hm_build_col_map(df, cm, c("G0", "HDL_c"), fn = "test")
  expect_true("G0" %in% names(res$data))
  expect_equal(res$data[["G0"]], c(5.5, 6.0))
  # original column still present
  expect_true("pglu0" %in% names(res$data))
})

test_that(".hm_build_col_map: non-clobbering — existing logical key not overwritten", {
  skip_on_cran()
  # data already has a column literally named "G0" with different values
  df  <- tibble::tibble(G0 = c(4.0, 5.0), pglu0 = c(9.9, 9.9))
  cm  <- list(G0 = "pglu0")
  res <- HealthMarkers:::.hm_build_col_map(df, cm, c("G0"), fn = "test")
  # G0 already existed -> should NOT be overwritten with pglu0 values
  expect_equal(res$data[["G0"]], c(4.0, 5.0))
})

test_that(".hm_build_col_map: literal-name fallback for unrecognised column names", {
  skip_on_cran()
  # "my_custom_col" is not in the dictionary, but the key is literally named the same
  df  <- tibble::tibble(my_custom_col = 1:3)
  res <- HealthMarkers:::.hm_build_col_map(df, NULL, c("my_custom_col"), fn = "test")
  expect_equal(res$col_map[["my_custom_col"]], "my_custom_col")
  expect_equal(res$inferred_keys, "my_custom_col")
})

test_that(".hm_build_col_map: synonym matching works end-to-end", {
  skip_on_cran()
  df  <- tibble::tibble(insu0 = c(60, 80), pglu0 = c(5.5, 6.0))
  res <- HealthMarkers:::.hm_build_col_map(df, NULL, c("I0", "G0"), fn = "test")
  expect_equal(res$col_map[["I0"]], "insu0")
  expect_equal(res$col_map[["G0"]], "pglu0")
})

## Tests for .hm_global_precompute() -----------------------------------------

test_that(".hm_global_precompute: BMI derived from weight (kg) and height (cm)", {
  skip_on_cran()
  df  <- data.frame(weight = 70.0, height = 170.0)
  res <- HealthMarkers:::.hm_global_precompute(df, col_map = NULL, verbose = FALSE)
  expect_true("BMI" %in% names(res$data))
  expect_equal(round(res$data[["BMI"]], 2), round(70 / 1.70^2, 2))
  expect_true("BMI" %in% res$precomputed)
})

test_that(".hm_global_precompute: G0 aliased to glucose when glucose absent", {
  skip_on_cran()
  df  <- data.frame(G0 = 5.5)
  res <- HealthMarkers:::.hm_global_precompute(df, col_map = NULL, verbose = FALSE)
  expect_true("glucose" %in% names(res$data))
  expect_equal(res$data[["glucose"]], 5.5)
  expect_true("glucose" %in% res$precomputed)
})

test_that(".hm_global_precompute: glucose aliased to G0 when G0 absent", {
  skip_on_cran()
  df  <- data.frame(glucose = 6.0)
  res <- HealthMarkers:::.hm_global_precompute(df, col_map = NULL, verbose = FALSE)
  expect_true("G0" %in% names(res$data))
  expect_equal(res$data[["G0"]], 6.0)
  expect_true("G0" %in% res$precomputed)
})

test_that(".hm_global_precompute: I0 aliased to insulin when insulin absent", {
  skip_on_cran()
  df  <- data.frame(I0 = 12.0)
  res <- HealthMarkers:::.hm_global_precompute(df, col_map = NULL, verbose = FALSE)
  expect_true("insulin" %in% names(res$data))
  expect_equal(res$data[["insulin"]], 12.0)
  expect_true("insulin" %in% res$precomputed)
})

test_that(".hm_global_precompute: eGFR derived from creatinine, age, sex", {
  skip_on_cran()
  df  <- data.frame(creatinine = 1.0, age = 40, sex = "M")
  res <- HealthMarkers:::.hm_global_precompute(df, col_map = NULL, verbose = FALSE)
  expect_true("eGFR" %in% names(res$data))
  expect_true(is.finite(res$data[["eGFR"]]))
  expect_true(res$data[["eGFR"]] > 60 && res$data[["eGFR"]] < 130)
  expect_true("eGFR" %in% res$precomputed)
})

test_that(".hm_global_precompute: UACR derived from urine_albumin / urine_creatinine", {
  skip_on_cran()
  df  <- data.frame(urine_albumin = 30.0, urine_creatinine = 150.0)
  res <- HealthMarkers:::.hm_global_precompute(df, col_map = NULL, verbose = FALSE)
  expect_true("UACR" %in% names(res$data))
  expect_equal(res$data[["UACR"]], 30.0 / 150.0)
  expect_true("UACR" %in% res$precomputed)
})

test_that(".hm_global_precompute: LDL_c derived via Friedewald (mmol/L)", {
  skip_on_cran()
  df  <- data.frame(TC = 5.0, HDL_c = 1.2, TG = 1.5)
  res <- HealthMarkers:::.hm_global_precompute(df, col_map = NULL, verbose = FALSE)
  expect_true("LDL_c" %in% names(res$data))
  expect_equal(round(res$data[["LDL_c"]], 4), round(5.0 - 1.2 - 1.5 / 2.2, 4))
  expect_true("LDL_c" %in% res$precomputed)
})

test_that(".hm_global_precompute: LDL_c is NA when TG > 4.5 mmol/L", {
  skip_on_cran()
  df  <- data.frame(TC = 5.0, HDL_c = 1.2, TG = 5.0)
  res <- HealthMarkers:::.hm_global_precompute(df, col_map = NULL, verbose = FALSE)
  expect_true("LDL_c" %in% names(res$data))
  expect_true(is.na(res$data[["LDL_c"]]))
})

test_that(".hm_global_precompute: existing columns are not overwritten", {
  skip_on_cran()
  df  <- data.frame(BMI = 30.0, weight = 50.0, height = 170.0)
  res <- HealthMarkers:::.hm_global_precompute(df, col_map = NULL, verbose = FALSE)
  expect_equal(res$data[["BMI"]], 30.0)
  expect_false("BMI" %in% res$precomputed)
})

test_that(".hm_global_precompute: col_map keys are honoured for column resolution", {
  skip_on_cran()
  df  <- data.frame(cr = 0.9, age = 50, sex = "F")
  cm  <- list(creatinine = "cr")
  res <- HealthMarkers:::.hm_global_precompute(df, col_map = cm, verbose = FALSE)
  expect_true("eGFR" %in% names(res$data))
  expect_true(is.finite(res$data[["eGFR"]]))
})
