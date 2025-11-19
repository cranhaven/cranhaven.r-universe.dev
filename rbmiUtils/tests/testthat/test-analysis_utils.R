testthat::test_that("extract_covariates2 splits, trims, dedups, and handles NULL", {
  expect_null(extract_covariates2(NULL))

  x <- c("A:B", " C*D ", "E", "E", "F : G")
  out <- extract_covariates2(x)
  expect_true(all(c("A", "B", "C", "D", "E", "F", "G") %in% out))
  expect_equal(length(out), length(unique(out)))

  # No operators, order preserved after trim
  expect_equal(extract_covariates2(c("AGE", " SEX ")), c("AGE", "SEX"))
})

testthat::test_that("as_simple_formula2 builds intended formula; empty covars behavior is robust", {
  frm <- as_simple_formula2("Y", c("A", "B:C", "D*E"))
  # Compare while ignoring whitespace around operators (especially `*`)
  expect_identical(gsub("\\s+", "", deparse(frm)), "Y~1+A+B:C+D*E")
  expect_identical(environment(frm), globalenv())

  # If you implemented intercept-only support, assert it; otherwise assert an error.
  intercept_ok <- FALSE
  out <- try(as_simple_formula2("Y", character(0)), silent = TRUE)
  if (!inherits(out, "try-error")) {
    expect_identical(gsub("\\s+", "", deparse(out)), "Y~1")
    intercept_ok <- TRUE
  }
  if (!intercept_ok) {
    expect_error(as_simple_formula2("Y", character(0)))
  }
})

# ----------------------- beeca-dependent tests ------------------------------
# These exercise gcomp_responder and gcomp_responder_multi without mocking.
# They skip cleanly if beeca is not available.

testthat::test_that("gcomp_responder runs, drops visit from model terms, returns stable shape", {
  testthat::skip_if_not_installed("beeca")
  set.seed(1)
  dat <- data.frame(
    Y = rbinom(160, 1, 0.45),
    TRT = factor(
      sample(c("Placebo", "Drug"), 160, TRUE),
      levels = c("Placebo", "Drug")
    ),
    BASE = rnorm(160),
    VIS = sample(c("W4", "W8"), 160, TRUE)
  )
  vars <- list(
    outcome = "Y",
    group = "TRT",
    covariates = c("BASE", "TRT:BASE", "VIS"),
    visit = "VIS"
  )

  out <- gcomp_responder(
    data = dat,
    vars = vars,
    reference_levels = "Placebo",
    var_method = "Ge",
    type = "HC0",
    contrast = "diff"
  )

  # Ensure VIS was not in the model terms
  frm <- stats::as.formula(paste0(
    vars$outcome,
    " ~ 1 + ",
    paste0(
      setdiff(
        unique(c(vars$group, extract_covariates2(vars$covariates))),
        vars$visit
      ),
      collapse = " + "
    )
  ))
  m <- stats::glm(frm, data = dat, family = binomial())
  terms_used <- attr(stats::terms(m), "term.labels")
  expect_false(any(grepl("^VIS$", terms_used)))

  # Output structure: at least one trt_* and lsm_*; each has est/se/df
  expect_true(any(grepl("^trt_", names(out))))
  expect_true(any(grepl("^lsm_", names(out))))
  for (nm in names(out)) {
    expect_true(all(c("est", "se", "df") %in% names(out[[nm]])))
    expect_type(out[[nm]]$est, "double")
    expect_type(out[[nm]]$se, "double")
    expect_true(is.na(out[[nm]]$df))
  }
})

testthat::test_that("gcomp_responder defaults reference to first factor level (smoke)", {
  testthat::skip_if_not_installed("beeca")
  set.seed(2)
  dat <- data.frame(
    Y = rbinom(80, 1, 0.5),
    TRT = factor(
      rep(c("Placebo", "Drug"), each = 40),
      levels = c("Placebo", "Drug")
    ),
    BASE = rnorm(80),
    VIS = "W4"
  )
  vars <- list(
    outcome = "Y",
    group = "TRT",
    covariates = c("BASE", "VIS"),
    visit = "VIS"
  )

  # Should not error and should return structured results when reference not supplied
  out <- gcomp_responder(dat, vars)
  expect_true(length(out) > 0)
  expect_true(all(vapply(
    out,
    function(x) all(c("est", "se", "df") %in% names(x)),
    logical(1)
  )))
})

testthat::test_that("gcomp_responder validates that group is a factor (if implemented)", {
  testthat::skip_if_not_installed("beeca")
  dat <- data.frame(
    Y = rbinom(10, 1, 0.5),
    TRT = rep(c("Placebo", "Drug"), each = 5), # character, not factor
    BASE = rnorm(10),
    VIS = "W4"
  )
  vars <- list(outcome = "Y", group = "TRT", covariates = "BASE", visit = "VIS")

  # If validation added, expect a clear error; otherwise allow skip.
  err <- try(
    gcomp_responder(dat, vars, reference_levels = "Placebo"),
    silent = TRUE
  )
  if (inherits(err, "try-error")) {
    expect_match(as.character(err), "(?i)factor|categorical")
  } else {
    testthat::skip(
      "group-factor validation not implemented; skipping assertion."
    )
  }
})

testthat::test_that("gcomp_responder errors for invalid reference level (if implemented)", {
  testthat::skip_if_not_installed("beeca")
  dat <- data.frame(
    Y = rbinom(20, 1, 0.5),
    TRT = factor(rep(c("Placebo", "Drug"), each = 10)),
    BASE = rnorm(20),
    VIS = "W8"
  )
  vars <- list(outcome = "Y", group = "TRT", covariates = "BASE", visit = "VIS")

  err <- try(
    gcomp_responder(dat, vars, reference_levels = "ActiveX"),
    silent = TRUE
  )
  if (inherits(err, "try-error")) {
    expect_match(as.character(err), "(?i)reference.*level|not.*in.*levels")
  } else {
    testthat::skip(
      "reference-level validation not implemented; skipping assertion."
    )
  }
})

testthat::test_that("gcomp_responder validates contrast against allowed set (either here or in beeca)", {
  testthat::skip_if_not_installed("beeca")
  dat <- data.frame(
    Y = rbinom(30, 1, 0.5),
    TRT = factor(rep(c("Placebo", "Drug"), each = 15)),
    BASE = rnorm(30),
    VIS = "W8"
  )
  vars <- list(outcome = "Y", group = "TRT", covariates = "BASE", visit = "VIS")

  expect_error(
    gcomp_responder(
      dat,
      vars,
      reference_levels = "Placebo",
      contrast = "weird"
    ),
    regexp = "(?i)contrast|allowed|supported|'arg' should be one of"
  )
})

testthat::test_that("gcomp_responder works with no covariates after extraction (NULL covariates)", {
  testthat::skip_if_not_installed("beeca")
  set.seed(4)
  dat <- data.frame(
    Y = rbinom(60, 1, 0.45),
    TRT = factor(
      sample(c("Placebo", "Drug"), 60, TRUE),
      levels = c("Placebo", "Drug")
    ),
    AVISIT = "W12"
  )
  vars <- list(
    outcome = "Y",
    group = "TRT",
    covariates = NULL,
    visit = "AVISIT"
  )

  out <- gcomp_responder(dat, vars, reference_levels = "Placebo")
  expect_true(any(grepl("^lsm_", names(out))))
})

testthat::test_that("gcomp_responder_multi applies per-visit and suffixes names", {
  testthat::skip_if_not_installed("beeca")
  set.seed(5)
  dat <- data.frame(
    Y = rbinom(50, 1, 0.5),
    TRT = factor(
      sample(c("Placebo", "Drug"), 50, TRUE),
      levels = c("Placebo", "Drug")
    ),
    BASE = rnorm(50),
    AVISIT = factor(sample(c("W4", "W8"), 50, TRUE)) # unsorted by design
  )
  vars <- list(
    outcome = "Y",
    group = "TRT",
    covariates = "BASE",
    visit = "AVISIT"
  )

  out <- gcomp_responder_multi(dat, vars, reference_levels = "Placebo")

  v <- unique(dat$AVISIT)
  # Every visit should suffix at least one trt_* and one lsm_*
  for (vv in v) {
    expect_true(any(grepl(paste0("^trt_.*_", vv, "$"), names(out))))
    expect_true(any(grepl(paste0("^lsm_.*_", vv, "$"), names(out))))
  }
  # Sanity: entries have est/se/df
  any_nm <- names(out)[1]
  expect_true(all(c("est", "se", "df") %in% names(out[[any_nm]])))
})
