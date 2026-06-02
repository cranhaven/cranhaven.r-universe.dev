## Integration tests for all_health_markers() — Phase E DAG pipeline
## Verifies that .hm_global_precompute() (Tier 0) feeds derived columns to
## downstream groups within the same all_health_markers() call.

# ---------------------------------------------------------------------------
# eGFR pipeline: creatinine + age + sex -> eGFR (Tier 0) -> ckd_stage (Tier 2)
# ---------------------------------------------------------------------------
test_that("all_health_markers: global precompute derives eGFR and feeds ckd_stage", {
  skip_on_cran()
  df <- data.frame(
    creatinine = c(0.9, 1.5),
    age        = c(45,  70),
    sex        = c("F", "M")
  )
  out <- suppressWarnings(all_health_markers(
    df,
    col_map        = list(),
    which          = c("renal", "ckd_stage"),
    include_insulin = FALSE,
    verbose        = FALSE,
    na_action      = "keep"
  ))
  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), 2L)
  # Global precompute must have added eGFR
  expect_true("eGFR" %in% names(out))
  expect_true(all(is.finite(out[["eGFR"]])))
  # ckd_stage downstream group must have run
  expect_true("CKD_stage" %in% names(out))
  expect_false(any(is.na(out[["CKD_stage"]])))
})

test_that("all_health_markers: ckd_stage resolves even without the 'renal' group", {
  skip_on_cran()
  # Only creatinine+age+sex — no renal group; eGFR comes solely from global precompute
  df <- data.frame(
    creatinine = c(1.2),
    age        = c(55),
    sex        = c("M")
  )
  out <- suppressWarnings(all_health_markers(
    df,
    col_map        = list(),
    which          = "ckd_stage",
    include_insulin = FALSE,
    verbose        = FALSE,
    na_action      = "keep"
  ))
  expect_true("eGFR"      %in% names(out))
  expect_true("CKD_stage" %in% names(out))
  expect_true(is.finite(out[["eGFR"]]))
})

# ---------------------------------------------------------------------------
# BMI pipeline: weight + height -> BMI (Tier 0) -> used by lipid/glycemic
# ---------------------------------------------------------------------------
test_that("all_health_markers: global precompute derives BMI from weight and height", {
  skip_on_cran()
  df <- data.frame(
    weight = 80.0, height = 175.0,
    TC = 5.2, HDL_c = 1.3, TG = 1.8
  )
  out <- suppressWarnings(all_health_markers(
    df,
    col_map        = list(),
    which          = "lipid",
    include_insulin = FALSE,
    verbose        = FALSE,
    na_action      = "keep"
  ))
  expect_true("BMI" %in% names(out))
  expect_equal(round(out[["BMI"]], 2), round(80 / (1.75^2), 2))
})

# ---------------------------------------------------------------------------
# LDL_c pipeline: TC + HDL_c + TG -> LDL_c (Tier 0) -> lipid_markers ratios
# ---------------------------------------------------------------------------
test_that("all_health_markers: global precompute derives LDL_c and lipid group uses it", {
  skip_on_cran()
  df <- data.frame(TC = 5.2, HDL_c = 1.3, TG = 1.8)
  out <- suppressWarnings(all_health_markers(
    df,
    col_map        = list(),
    which          = "lipid",
    include_insulin = FALSE,
    verbose        = FALSE,
    na_action      = "keep"
  ))
  expect_true("LDL_c" %in% names(out))
  expect_true(is.finite(out[["LDL_c"]]))
  # LDL_c / HDL_c ratio should also exist (produced by lipid_markers)
  expect_true(any(grepl("LDL", names(out))))
})

# ---------------------------------------------------------------------------
# Glucose alias pipeline: G0 -> glucose (Tier 0) -> glycemic group
# ---------------------------------------------------------------------------
test_that("all_health_markers: G0 aliased to glucose by global precompute", {
  skip_on_cran()
  df <- data.frame(G0 = 5.5, I0 = 10.0, age = 35, sex = "M", BMI = 25)
  out <- suppressWarnings(all_health_markers(
    df,
    col_map        = list(G0 = "G0", I0 = "I0"),
    which          = "glycemic",
    include_insulin = FALSE,
    verbose        = FALSE,
    na_action      = "keep"
  ))
  expect_true("glucose" %in% names(out))
  expect_equal(out[["glucose"]], 5.5)
})

# ---------------------------------------------------------------------------
# UACR pipeline: urine_albumin + urine_creatinine -> UACR (Tier 0) -> ckd_stage
# ---------------------------------------------------------------------------
test_that("all_health_markers: UACR derived by global precompute and used by ckd_stage", {
  skip_on_cran()
  df <- data.frame(
    creatinine       = 1.1,
    age              = 60,
    sex              = "F",
    urine_albumin    = 45.0,
    urine_creatinine = 150.0
  )
  out <- suppressWarnings(all_health_markers(
    df,
    col_map        = list(),
    which          = "ckd_stage",
    include_insulin = FALSE,
    verbose        = FALSE,
    na_action      = "keep"
  ))
  expect_true("UACR" %in% names(out))
  expect_equal(out[["UACR"]], 45.0 / 150.0)
  expect_true("Albuminuria_stage" %in% names(out))
  # UACR = 0.3 -> A1 category
  expect_equal(as.character(out[["Albuminuria_stage"]]), "A1")
})

# ---------------------------------------------------------------------------
# No-data graceful skip: ckd_stage absent when no eGFR inputs provided
# ---------------------------------------------------------------------------
test_that("all_health_markers: ckd_stage skipped gracefully when no eGFR data", {
  skip_on_cran()
  df <- data.frame(TC = 5.0, HDL_c = 1.2, TG = 1.8)
  out <- suppressWarnings(all_health_markers(
    df,
    col_map        = list(),
    which          = c("lipid", "ckd_stage"),
    include_insulin = FALSE,
    verbose        = FALSE,
    na_action      = "keep"
  ))
  # lipid should have run
  expect_true("non_HDL_c" %in% names(out) || any(grepl("LDL|HDL|TC", names(out))))
  # ckd_stage should NOT have produced CKD_stage (no creatinine in data)
  expect_false("CKD_stage" %in% names(out))
})

# ---------------------------------------------------------------------------
# .hm_safe_call() error attribute structure
# ---------------------------------------------------------------------------
test_that(".hm_safe_call() returns hm_error attribute when function throws", {
  skip_on_cran()
  bad_fun <- function(data, ...) stop("deliberate test failure")
  result <- HealthMarkers:::.hm_safe_call(
    fun           = bad_fun,
    data          = data.frame(x = 1),
    col_map       = list(),
    needs_col_map = FALSE,
    verbose       = FALSE,
    tag           = "test_tag"
  )
  expect_false(is.null(attr(result, "hm_error")))
  expect_match(attr(result, "hm_error"), "deliberate test failure")
})

test_that(".hm_safe_call() captures missing-package error and sets hm_missing_pkg", {
  skip_on_cran()
  pkg_fun <- function(data, ...) {
    rlang::abort("Package 'fakepackage' is required for this feature.",
                 class = "healthmarkers_missing_package", package = "fakepackage")
  }
  result <- HealthMarkers:::.hm_safe_call(
    fun           = pkg_fun,
    data          = data.frame(x = 1),
    col_map       = list(),
    needs_col_map = FALSE,
    verbose       = FALSE,
    tag           = "pkg_test"
  )
  expect_equal(attr(result, "hm_missing_pkg"), "fakepackage")
})
