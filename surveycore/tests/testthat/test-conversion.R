# tests/testthat/test-conversion.R
#
# Tests for conversion functions in R/05-methods-conversion.R.
# Step 11: as_svydesign(), as_tbl_svy()
# Step 12: from_svydesign(), from_tbl_svy()
#
# All blocks that exercise survey/srvyr use skip_if_not_installed().
#
# Test structure:
#   as_svydesign() — happy paths
#     1. survey_taylor (stratified cluster design)
#     2. survey_taylor (SRS / no design vars)
#     3. survey_replicate (BRR)
#     4. survey_twophase
#   as_svydesign() — output structure
#     5. output class for survey_taylor
#     6. output class for survey_replicate
#     7. output class for survey_twophase
#   as_svydesign() — data preserved
#     8. data rows match
#     9. data columns preserved (superset of original)
#   as_svydesign() — estimation matches survey package
#    10. svymean agrees for survey_taylor [numerical]
#    11. svymean agrees for survey_replicate [numerical]
#   as_svydesign() — error on non-survey input
#    12. rejects plain data.frame
#   as_tbl_svy() — happy path
#    13. survey_taylor → tbl_svy
#   as_tbl_svy() — error on non-survey input
#    14. rejects plain data.frame
#   from_svydesign() — happy paths
#    15. svydesign (Taylor) → survey_taylor
#    16. svyrep.design (BRR) → survey_replicate
#    17. twophase → survey_twophase
#   from_svydesign() — output class
#    18. returns survey_taylor for svydesign
#    19. returns survey_replicate for svyrep.design
#    20. returns survey_twophase for twophase
#   from_svydesign() — data preserved
#    21. data rows match
#    22. original columns preserved in @data
#   from_svydesign() — design variables recovered
#    23. strata column recovered from Taylor design
#    24. repweight columns recovered from replicate design
#   from_svydesign() — numerical round-trip
#    25. svymean round-trip for Taylor design [numerical]
#    26. svymean round-trip for replicate design [numerical]
#   from_svydesign() — error on non-survey input
#    27. rejects plain data.frame
#   as_svydesign() — survey_taylor with no ids uses ids = ~1
#    37. as_svydesign(survey_taylor) with ids = NULL produces ids = ~1
#   from_svydesign() — twophase method unknown fallback warning
#    38. warns when twophase x$method is unrecognised
#   from_tbl_svy() — happy path
#    28. tbl_svy → survey_taylor
#   from_tbl_svy() — error on non-tbl_svy input
#    29. rejects plain data.frame

# ── Fixtures ─────────────────────────────────────────────────────────────────

make_taylor <- function(seed = 42L) {
  df <- make_survey_data(n = 50L, n_psu = 10L, n_strata = 2L, seed = seed)
  as_survey(
    df,
    ids = psu,
    weights = wt,
    strata = strata,
    fpc = fpc,
    nest = TRUE
  )
}

make_srs <- function(seed = 42L) {
  df <- make_survey_data(n = 30L, n_psu = 10L, n_strata = 2L, seed = seed)
  as_survey(df, weights = wt)
}

make_rep <- function(seed = 42L) {
  df <- make_survey_data(
    n = 50L,
    n_psu = 10L,
    n_strata = 2L,
    design = "replicate",
    type = "brr",
    seed = seed
  )
  repwt_cols <- grep("^repwt_", names(df), value = TRUE)
  as_survey_replicate(
    df,
    weights = wt,
    repweights = tidyselect::all_of(repwt_cols),
    type = "BRR"
  )
}

make_twophase <- function(seed = 42L) {
  df <- make_survey_data(
    n = 60L,
    n_psu = 10L,
    n_strata = 2L,
    design = "twophase",
    seed = seed
  )
  phase1 <- as_survey(
    df,
    ids = psu,
    weights = wt,
    strata = strata,
    fpc = fpc,
    nest = TRUE
  )
  as_survey_twophase(phase1, subset = subset)
}


# ── 1. as_svydesign() — survey_taylor happy path ─────────────────────────────

test_that("as_svydesign() converts survey_taylor without error", {
  skip_if_not_installed("survey")
  d <- make_taylor()
  test_invariants(d)
  sv <- as_svydesign(d)
  expect_true(!is.null(sv))
})


# ── 2. as_svydesign() — SRS survey_taylor happy path ─────────────────────────

test_that("as_svydesign() converts SRS-style survey_taylor without error", {
  skip_if_not_installed("survey")
  d <- make_srs()
  test_invariants(d)
  sv <- as_svydesign(d)
  expect_true(!is.null(sv))
})


# ── 3. as_svydesign() — survey_replicate happy path ──────────────────────────

test_that("as_svydesign() converts survey_replicate without error", {
  skip_if_not_installed("survey")
  d <- make_rep()
  test_invariants(d)
  sv <- as_svydesign(d)
  expect_true(!is.null(sv))
})


# ── 4. as_svydesign() — survey_twophase happy path ───────────────────────────

test_that("as_svydesign() converts survey_twophase without error", {
  skip_if_not_installed("survey")
  d <- make_twophase()
  test_invariants(d)
  sv <- suppressWarnings(as_svydesign(d))
  expect_true(!is.null(sv))
})


# ── 5–7. as_svydesign() — output class ───────────────────────────────────────

test_that("as_svydesign(survey_taylor) returns a survey.design2 object", {
  skip_if_not_installed("survey")
  d <- make_taylor()
  sv <- as_svydesign(d)
  expect_true(inherits(sv, "survey.design"))
})

test_that("as_svydesign(survey_replicate) returns a svyrep.design object", {
  skip_if_not_installed("survey")
  d <- make_rep()
  sv <- as_svydesign(d)
  expect_true(inherits(sv, "svyrep.design"))
})

test_that("as_svydesign(survey_twophase) returns a twophase2 survey.design object", {
  skip_if_not_installed("survey")
  d <- make_twophase()
  sv <- suppressWarnings(as_svydesign(d))
  # survey::twophase() with method="full" returns class "twophase2"; the
  # broader "survey.design" class is always present for two-phase designs.
  expect_true(inherits(sv, "survey.design"))
})


# ── 8–9. as_svydesign() — data preserved ─────────────────────────────────────

test_that("as_svydesign() preserves all data rows", {
  skip_if_not_installed("survey")
  d <- make_taylor()
  sv <- as_svydesign(d)
  expect_identical(nrow(sv$variables), nrow(d@data))
})

test_that("as_svydesign() preserves all original columns in output data", {
  skip_if_not_installed("survey")
  d <- make_taylor()
  sv <- as_svydesign(d)
  expect_true(all(names(d@data) %in% names(sv$variables)))
})


# ── 10. as_svydesign() — svymean numerical agreement: survey_taylor ──────────

test_that("as_svydesign(survey_taylor) gives svymean matching survey::svydesign [numerical]", {
  skip_if_not_installed("survey")
  df <- make_survey_data(n = 50L, n_psu = 10L, n_strata = 2L, seed = 99L)

  d_sc <- as_survey(
    df,
    ids = psu,
    weights = wt,
    strata = strata,
    fpc = fpc,
    nest = TRUE
  )
  d_sv <- survey::svydesign(
    ids = ~psu,
    weights = ~wt,
    strata = ~strata,
    fpc = ~fpc,
    data = df,
    nest = TRUE
  )

  sc_mean <- survey::svymean(~y1, as_svydesign(d_sc))
  sv_mean <- survey::svymean(~y1, d_sv)

  expect_equal(coef(sc_mean)[["y1"]], coef(sv_mean)[["y1"]], tolerance = 1e-10)
  # survey::SE() returns a 1×1 matrix in survey >= 4.4; use as.numeric() for
  # robust indexing across package versions.
  expect_equal(
    as.numeric(survey::SE(sc_mean)),
    as.numeric(survey::SE(sv_mean)),
    tolerance = 1e-8
  )
})


# ── 11. as_svydesign() — svymean numerical agreement: survey_replicate ────────

test_that("as_svydesign(survey_replicate) gives svymean matching survey::svrepdesign [numerical]", {
  skip_if_not_installed("survey")
  df <- make_survey_data(
    n = 50L,
    n_psu = 10L,
    n_strata = 2L,
    design = "replicate",
    type = "brr",
    seed = 99L
  )
  repwt_cols <- grep("^repwt_", names(df), value = TRUE)

  d_sc <- as_survey_replicate(
    df,
    weights = wt,
    repweights = tidyselect::all_of(repwt_cols),
    type = "BRR"
  )
  d_sv <- survey::svrepdesign(
    weights = df$wt,
    repweights = df[, repwt_cols],
    type = "BRR",
    mse = TRUE, # as_survey_replicate() defaults to mse = TRUE
    data = df
  )

  sc_mean <- survey::svymean(~y1, as_svydesign(d_sc))
  sv_mean <- survey::svymean(~y1, d_sv)

  expect_equal(coef(sc_mean)[["y1"]], coef(sv_mean)[["y1"]], tolerance = 1e-10)
  # survey::SE() returns a 1×1 matrix in survey >= 4.4; use as.numeric() for
  # robust indexing across package versions.
  expect_equal(
    as.numeric(survey::SE(sc_mean)),
    as.numeric(survey::SE(sv_mean)),
    tolerance = 1e-8
  )
})


# ── 12. as_svydesign() — error on non-survey input ───────────────────────────

test_that("as_svydesign() rejects a plain data.frame", {
  skip_if_not_installed("survey")
  expect_error(
    as_svydesign(data.frame(x = 1)),
    class = "surveycore_error_not_survey_object"
  )
})


# ── 13. as_tbl_svy() — survey_taylor → tbl_svy ───────────────────────────────

test_that("as_tbl_svy() converts survey_taylor to tbl_svy", {
  skip_if_not_installed("survey")
  skip_if_not_installed("srvyr")
  d <- make_taylor()
  test_invariants(d)
  ts <- as_tbl_svy(d)
  expect_true(inherits(ts, "tbl_svy"))
})

test_that("as_tbl_svy() preserves all data rows", {
  skip_if_not_installed("survey")
  skip_if_not_installed("srvyr")
  d <- make_taylor()
  ts <- as_tbl_svy(d)
  expect_identical(nrow(ts), nrow(d@data))
})


# ── 14. as_tbl_svy() — error on non-survey input ─────────────────────────────

test_that("as_tbl_svy() rejects a plain data.frame", {
  skip_if_not_installed("survey")
  skip_if_not_installed("srvyr")
  expect_error(
    as_tbl_svy(data.frame(x = 1)),
    class = "surveycore_error_not_survey_object"
  )
})


# ── 15–17. from_svydesign() — happy paths ────────────────────────────────────

test_that("from_svydesign() converts svydesign to survey_taylor without error", {
  skip_if_not_installed("survey")
  df <- make_survey_data(n = 50L, n_psu = 10L, n_strata = 2L, seed = 42L)
  sv <- survey::svydesign(
    ids = ~psu,
    weights = ~wt,
    strata = ~strata,
    fpc = ~fpc,
    data = df,
    nest = TRUE
  )
  d <- from_svydesign(sv)
  expect_true(!is.null(d))
})

test_that("from_svydesign() converts svyrep.design to survey_replicate without error", {
  skip_if_not_installed("survey")
  df <- make_survey_data(
    n = 50L,
    n_psu = 10L,
    n_strata = 2L,
    design = "replicate",
    type = "brr",
    seed = 42L
  )
  repwt_cols <- grep("^repwt_", names(df), value = TRUE)
  sv <- survey::svrepdesign(
    weights = df$wt,
    repweights = df[, repwt_cols],
    type = "BRR",
    mse = TRUE,
    data = df
  )
  d <- from_svydesign(sv)
  expect_true(!is.null(d))
})

test_that("from_svydesign() converts twophase design to survey_twophase without error", {
  skip_if_not_installed("survey")
  df <- make_survey_data(
    n = 60L,
    n_psu = 10L,
    n_strata = 2L,
    design = "twophase",
    seed = 42L
  )
  phase1 <- as_survey(
    df,
    ids = psu,
    weights = wt,
    strata = strata,
    fpc = fpc,
    nest = TRUE
  )
  d_sc <- as_survey_twophase(phase1, subset = subset)
  sv_tp <- suppressWarnings(as_svydesign(d_sc))
  d <- suppressWarnings(from_svydesign(sv_tp))
  expect_true(!is.null(d))
})


# ── 18–20. from_svydesign() — output class ───────────────────────────────────

test_that("from_svydesign(svydesign) returns a survey_taylor object", {
  skip_if_not_installed("survey")
  df <- make_survey_data(n = 50L, n_psu = 10L, n_strata = 2L, seed = 42L)
  sv <- survey::svydesign(
    ids = ~psu,
    weights = ~wt,
    strata = ~strata,
    fpc = ~fpc,
    data = df,
    nest = TRUE
  )
  d <- from_svydesign(sv)
  expect_true(S7::S7_inherits(d, survey_taylor))
})

test_that("from_svydesign(svyrep.design) returns a survey_replicate object", {
  skip_if_not_installed("survey")
  df <- make_survey_data(
    n = 50L,
    n_psu = 10L,
    n_strata = 2L,
    design = "replicate",
    type = "brr",
    seed = 42L
  )
  repwt_cols <- grep("^repwt_", names(df), value = TRUE)
  sv <- survey::svrepdesign(
    weights = df$wt,
    repweights = df[, repwt_cols],
    type = "BRR",
    mse = TRUE,
    data = df
  )
  d <- from_svydesign(sv)
  expect_true(S7::S7_inherits(d, survey_replicate))
})

test_that("from_svydesign(twophase) returns a survey_twophase object", {
  skip_if_not_installed("survey")
  df <- make_survey_data(
    n = 60L,
    n_psu = 10L,
    n_strata = 2L,
    design = "twophase",
    seed = 42L
  )
  phase1 <- as_survey(
    df,
    ids = psu,
    weights = wt,
    strata = strata,
    fpc = fpc,
    nest = TRUE
  )
  d_sc <- as_survey_twophase(phase1, subset = subset)
  sv_tp <- suppressWarnings(as_svydesign(d_sc))
  d <- suppressWarnings(from_svydesign(sv_tp))
  expect_true(S7::S7_inherits(d, survey_twophase))
})


# ── 21–22. from_svydesign() — data preserved ─────────────────────────────────

test_that("from_svydesign() preserves all data rows", {
  skip_if_not_installed("survey")
  df <- make_survey_data(n = 50L, n_psu = 10L, n_strata = 2L, seed = 42L)
  sv <- survey::svydesign(
    ids = ~psu,
    weights = ~wt,
    strata = ~strata,
    fpc = ~fpc,
    data = df,
    nest = TRUE
  )
  d <- from_svydesign(sv)
  test_invariants(d)
  expect_identical(nrow(d@data), nrow(df))
})

test_that("from_svydesign() preserves original columns in @data", {
  skip_if_not_installed("survey")
  df <- make_survey_data(n = 50L, n_psu = 10L, n_strata = 2L, seed = 42L)
  sv <- survey::svydesign(
    ids = ~psu,
    weights = ~wt,
    strata = ~strata,
    fpc = ~fpc,
    data = df,
    nest = TRUE
  )
  d <- from_svydesign(sv)
  expect_true(all(names(df) %in% names(d@data)))
})


# ── 23–24. from_svydesign() — design variables recovered ─────────────────────

test_that("from_svydesign() recovers strata column name from Taylor design", {
  skip_if_not_installed("survey")
  df <- make_survey_data(n = 50L, n_psu = 10L, n_strata = 2L, seed = 42L)
  sv <- survey::svydesign(
    ids = ~psu,
    weights = ~wt,
    strata = ~strata,
    fpc = ~fpc,
    data = df,
    nest = TRUE
  )
  d <- from_svydesign(sv)
  expect_identical(d@variables$strata, "strata")
})

test_that("from_svydesign() recovers replicate weight column names", {
  skip_if_not_installed("survey")
  df <- make_survey_data(
    n = 50L,
    n_psu = 10L,
    n_strata = 2L,
    design = "replicate",
    type = "brr",
    seed = 42L
  )
  repwt_cols <- grep("^repwt_", names(df), value = TRUE)
  sv <- survey::svrepdesign(
    weights = df$wt,
    repweights = df[, repwt_cols],
    type = "BRR",
    mse = TRUE,
    data = df
  )
  d <- from_svydesign(sv)
  expect_identical(d@variables$repweights, repwt_cols)
})


# ── 25–26. from_svydesign() — numerical round-trip ───────────────────────────

test_that("from_svydesign() + as_svydesign() Taylor round-trip agrees [numerical]", {
  skip_if_not_installed("survey")
  df <- make_survey_data(n = 50L, n_psu = 10L, n_strata = 2L, seed = 77L)

  # Direct survey design
  sv_direct <- survey::svydesign(
    ids = ~psu,
    weights = ~wt,
    strata = ~strata,
    fpc = ~fpc,
    data = df,
    nest = TRUE
  )

  # Round-trip: survey → surveycore → survey
  d_sc <- from_svydesign(sv_direct)
  sv_rt <- as_svydesign(d_sc)

  m_direct <- survey::svymean(~y1, sv_direct)
  m_rt <- survey::svymean(~y1, sv_rt)

  expect_equal(coef(m_direct)[["y1"]], coef(m_rt)[["y1"]], tolerance = 1e-10)
  expect_equal(
    as.numeric(survey::SE(m_direct)),
    as.numeric(survey::SE(m_rt)),
    tolerance = 1e-8
  )
})

test_that("from_svydesign() + as_svydesign() replicate round-trip agrees [numerical]", {
  skip_if_not_installed("survey")
  df <- make_survey_data(
    n = 50L,
    n_psu = 10L,
    n_strata = 2L,
    design = "replicate",
    type = "brr",
    seed = 77L
  )
  repwt_cols <- grep("^repwt_", names(df), value = TRUE)

  sv_direct <- survey::svrepdesign(
    weights = df$wt,
    repweights = df[, repwt_cols],
    type = "BRR",
    mse = TRUE,
    data = df
  )

  d_sc <- from_svydesign(sv_direct)
  sv_rt <- as_svydesign(d_sc)

  m_direct <- survey::svymean(~y1, sv_direct)
  m_rt <- survey::svymean(~y1, sv_rt)

  expect_equal(coef(m_direct)[["y1"]], coef(m_rt)[["y1"]], tolerance = 1e-10)
  expect_equal(
    as.numeric(survey::SE(m_direct)),
    as.numeric(survey::SE(m_rt)),
    tolerance = 1e-8
  )
})


# ── 27. from_svydesign() — error on non-survey input ─────────────────────────

test_that("from_svydesign() rejects a plain data.frame", {
  skip_if_not_installed("survey")
  expect_error(
    from_svydesign(data.frame(x = 1)),
    class = "surveycore_error_not_survey_design"
  )
})


# ── 28. from_tbl_svy() — happy path ──────────────────────────────────────────

test_that("from_tbl_svy() converts tbl_svy to survey_taylor", {
  skip_if_not_installed("survey")
  skip_if_not_installed("srvyr")
  d <- make_taylor()
  test_invariants(d)
  ts <- as_tbl_svy(d)
  d2 <- from_tbl_svy(ts)
  expect_true(S7::S7_inherits(d2, survey_taylor))
  expect_identical(nrow(d2@data), nrow(d@data))
})


# ── 29. from_tbl_svy() — error on non-tbl_svy input ─────────────────────────

test_that("from_tbl_svy() rejects a plain data.frame", {
  skip_if_not_installed("survey")
  skip_if_not_installed("srvyr")
  expect_error(
    from_tbl_svy(data.frame(x = 1)),
    class = "surveycore_error_not_tbl_svy"
  )
})


# ── Coverage additions ────────────────────────────────────────────────────────

# 30. as_svydesign() for JK1 replicate design (scale_arg non-NULL path, line 129)
test_that("as_svydesign() converts JK1 replicate design (scale arg non-NULL path)", {
  skip_if_not_installed("survey")
  df <- make_survey_data(
    n = 50L,
    n_psu = 10L,
    n_strata = 2L,
    design = "replicate",
    type = "jk1",
    seed = 300L
  )
  repwt_cols <- grep("^repwt_", names(df), value = TRUE)
  d <- as_survey_replicate(
    df,
    weights = wt,
    repweights = tidyselect::all_of(repwt_cols),
    type = "JK1"
  )
  sv <- as_svydesign(d)
  expect_true(inherits(sv, "svyrep.design"))
})

# 31. as_svydesign() for twophase with SRS phase1 (p1$ids NULL → ~1, line 158)
test_that("as_svydesign() handles twophase design with SRS phase1 (no ids)", {
  skip_if_not_installed("survey")
  df <- make_survey_data(n = 100L, design = "twophase", seed = 301L)
  # phase1 with no ids (SRS-like) — p1$ids will be NULL
  phase1 <- as_survey(df, weights = wt, strata = strata)
  d2 <- as_survey_twophase(phase1, subset = subset)
  sv <- suppressWarnings(as_svydesign(d2))
  expect_true(inherits(sv, "survey.design"))
})

# 32. as_svydesign() for twophase with phase2 ids (p2$ids non-NULL → line 166)
test_that("as_svydesign() handles twophase design with phase2 ids", {
  skip_if_not_installed("survey")
  df <- make_survey_data(n = 100L, design = "twophase", seed = 302L)
  phase1 <- as_survey(df, weights = wt, strata = strata)
  d2 <- as_survey_twophase(phase1, ids2 = psu, subset = subset)
  sv <- suppressWarnings(as_svydesign(d2))
  expect_true(inherits(sv, "survey.design"))
})

# 33. from_svydesign() for SRS design covers .vars_from_formula(~1) → NULL (line 265)
test_that("from_svydesign() handles SRS design with ids = ~1 (.vars_from_formula NULL path)", {
  skip_if_not_installed("survey")
  set.seed(42)
  df <- data.frame(y = rnorm(50), w = runif(50, 0.5, 2))
  sv <- survey::svydesign(ids = ~1, weights = ~w, data = df)
  d <- from_svydesign(sv)
  expect_true(S7::S7_inherits(d, survey_taylor))
  expect_null(d@variables$ids)
})

# 34. from_svydesign() for replicate with external weight vector (lines 413-414)
test_that("from_svydesign() for replicate with external weights adds ..surveycore_wt.. column", {
  skip_if_not_installed("survey")
  set.seed(42)
  n <- 20L
  df <- data.frame(y = rnorm(n)) # no weight column in data
  ext_wts <- runif(n, 0.5, 2) # external weight vector
  rep_data <- matrix(runif(n * 4L), ncol = 4L)
  sv <- survey::svrepdesign(
    data = df,
    weights = ext_wts, # passed as vector, not formula — won't be in data
    repweights = rep_data,
    type = "BRR",
    combined.weights = TRUE
  )
  d <- from_svydesign(sv)
  expect_true(S7::S7_inherits(d, survey_replicate))
  # The fallback weight column should be added
  expect_true("..surveycore_wt.." %in% names(d@data))
})

# 35. from_svydesign() for twophase with inline subset (lines 447-448 fallback)
test_that("from_svydesign() twophase with inline subset creates ..surveycore_subset.. column", {
  skip_if_not_installed("survey")
  set.seed(42)
  n <- 30L
  df <- data.frame(x = rnorm(n), wt = rep(1, n))
  # subset is an inline logical vector — not a column in df, so .find_col_by_value returns NULL
  sub_lgl <- sample(c(TRUE, FALSE), n, replace = TRUE, prob = c(0.5, 0.5))
  sv <- survey::twophase(
    id = list(~1, ~1),
    strata = list(NULL, NULL),
    probs = list(NULL, NULL),
    data = df,
    subset = sub_lgl
  )
  d <- suppressWarnings(from_svydesign(sv))
  expect_true(S7::S7_inherits(d, survey_twophase))
  # The fallback subset column must have been created
  expect_true("..surveycore_subset.." %in% names(d@data))
})

# 36. visible_vars present after round-trip (from_svydesign Taylor and replicate)
test_that("from_svydesign() taylor result has visible_vars key in @variables", {
  skip_if_not_installed("survey")
  set.seed(42)
  df <- data.frame(x = rnorm(30), w = runif(30, 0.5, 2), s = rep(1:3, 10))
  sv <- survey::svydesign(ids = ~1, weights = ~w, strata = ~s, data = df)
  d <- from_svydesign(sv)
  expect_true("visible_vars" %in% names(d@variables))
})

test_that("from_svydesign() replicate result has visible_vars key in @variables", {
  skip_if_not_installed("survey")
  set.seed(42)
  n <- 20L
  df <- data.frame(y = rnorm(n), wt = runif(n, 1, 3))
  rep <- matrix(runif(n * 4L), ncol = 4L)
  sv <- survey::svrepdesign(
    data = df,
    weights = ~wt,
    repweights = rep,
    type = "BRR",
    combined.weights = TRUE
  )
  d <- from_svydesign(sv)
  expect_true("visible_vars" %in% names(d@variables))
})


# 37. as_svydesign() for survey_taylor with ids = NULL uses ids = ~1 (line 111)
test_that("as_svydesign() uses ids = ~1 when survey_taylor has ids = NULL", {
  skip_if_not_installed("survey")
  df <- make_survey_data(n = 30L, n_psu = 10L, n_strata = 2L, seed = 42L)
  # Stratified design with no cluster ids — ids = NULL in @variables
  d <- as_survey(df, weights = wt, strata = strata)
  test_invariants(d)
  expect_null(d@variables$ids)
  sv <- as_svydesign(d)
  expect_true(inherits(sv, "survey.design"))
})


# 38. from_svydesign() warns when twophase x$method is unrecognised (lines 465-472)
test_that("from_svydesign() warns surveycore_warning_twophase_method_unknown for unknown method", {
  skip_if_not_installed("survey")
  set.seed(42)
  n <- 30L
  df <- data.frame(x = rnorm(n), wt = rep(1, n))
  sub <- sample(c(TRUE, FALSE), n, replace = TRUE, prob = c(0.6, 0.4))
  # method = "approx" creates a plain "twophase" object (not "twophase2"), so
  # the class-based fallback does not fire and only x$method controls dispatch.
  sv <- survey::twophase(
    id = list(~1, ~1),
    strata = list(NULL, NULL),
    probs = list(NULL, NULL),
    data = df,
    subset = sub,
    method = "approx"
  )
  # Replace method with an unrecognised value to trigger the fallback warning.
  sv$method <- "nonexistent_method"

  expect_warning(
    d <- from_svydesign(sv),
    class = "surveycore_warning_twophase_method_unknown"
  )
  expect_true(S7::S7_inherits(d, survey_twophase))
  expect_identical(d@variables$method, "approx")
})
