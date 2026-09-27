# tests/testthat/test-s7-classes.R
#
# Tests for S7 class definitions in R/00-s7-classes.R.
#
# Coverage: error-messages.md rows 31–35 (S7 Layer 1 validator errors).
# Per testing-standards.md: validator errors use expect_error(class=) only
# — no expect_snapshot(), because S7 validator messages are not CLI-formatted
# the same way as Layer 3 constructor errors.

# ── Helpers ────────────────────────────────────────────────────────────────────

# Build a minimal variables list for survey_taylor with all keys present.
.taylor_vars <- function(
  weights = "wt",
  ids = NULL,
  strata = NULL,
  fpc = NULL,
  nest = FALSE,
  probs_provided = FALSE
) {
  list(
    ids = ids,
    weights = weights,
    strata = strata,
    fpc = fpc,
    nest = nest,
    probs_provided = probs_provided,
    visible_vars = NULL
  )
}

# Build a minimal variables list for survey_replicate with all keys present.
.rep_vars <- function(
  weights = "wt",
  repweights = c("rw1", "rw2"),
  type = "BRR",
  scale = 1,
  rscales = NULL,
  fpc = NULL,
  fpctype = "fraction",
  mse = TRUE
) {
  list(
    weights = weights,
    repweights = repweights,
    type = type,
    scale = scale,
    rscales = rscales,
    fpc = fpc,
    fpctype = fpctype,
    mse = mse,
    visible_vars = NULL
  )
}

# A small valid data frame for survey_taylor tests.
.df10 <- function(seed = 42L) {
  set.seed(seed)
  data.frame(
    psu = paste0("psu_", rep(1:5, 2)),
    strata = paste0("s", rep(1:2, each = 5)),
    fpc = rep(c(500L, 600L), each = 5),
    wt = runif(10, 0.5, 2),
    y = rnorm(10),
    stringsAsFactors = FALSE
  )
}

# A small valid data frame for survey_replicate tests.
.df_rep <- function(R = 4L, seed = 42L) {
  set.seed(seed)
  df <- data.frame(
    wt = runif(20, 0.5, 2),
    y = rnorm(20),
    stringsAsFactors = FALSE
  )
  for (i in seq_len(R)) {
    df[[paste0("rw", i)]] <- runif(20, 0.3, 3)
  }
  df
}


# ── survey_metadata ────────────────────────────────────────────────────────────

test_that("survey_metadata() creates object with empty default lists", {
  m <- survey_metadata()
  expect_true(S7::S7_inherits(m, survey_metadata))
  expect_identical(m@variable_labels, list())
  expect_identical(m@value_labels, list())
  expect_identical(m@question_prefaces, list())
  expect_identical(m@notes, list())
  expect_identical(m@transformations, list())
})

test_that("survey_metadata() stores variable labels correctly", {
  m <- survey_metadata(
    variable_labels = list(age = "Age in years", income = "Annual income")
  )
  expect_identical(m@variable_labels$age, "Age in years")
  expect_identical(m@variable_labels$income, "Annual income")
})

test_that("survey_metadata() stores value labels correctly", {
  m <- survey_metadata(
    value_labels = list(sex = c(Male = 1L, Female = 2L))
  )
  expect_identical(m@value_labels$sex, c(Male = 1L, Female = 2L))
})

test_that("survey_metadata() stores universe as empty list by default", {
  m <- survey_metadata()
  expect_identical(m@universe, list())
})

test_that("survey_metadata() stores missing_codes as empty list by default", {
  m <- survey_metadata()
  expect_identical(m@missing_codes, list())
})

test_that("survey_metadata(universe = list(...)) stores value correctly", {
  m <- survey_metadata(
    universe = list(age = "Adults 18+", income = "All respondents")
  )
  expect_identical(m@universe$age, "Adults 18+")
  expect_identical(m@universe$income, "All respondents")
})

test_that("survey_metadata(missing_codes = list(...)) stores value correctly", {
  m <- survey_metadata(missing_codes = list(age = c(Refused = 99L, DK = 98L)))
  expect_identical(m@missing_codes$age, c(Refused = 99L, DK = 98L))
})

test_that("universe @ assignment round-trips on survey_metadata object", {
  m <- survey_metadata()
  m@universe <- list(income = "Employed adults")
  expect_identical(m@universe$income, "Employed adults")
})

test_that("missing_codes @ assignment round-trips on survey_metadata object", {
  m <- survey_metadata()
  m@missing_codes <- list(age = c(-1L, -2L))
  expect_identical(m@missing_codes$age, c(-1L, -2L))
})


# ── survey_base ────────────────────────────────────────────────────────────────

test_that("survey_base cannot be instantiated directly (abstract class)", {
  df <- data.frame(x = 1)
  expect_error(
    survey_base(data = df),
    regexp = "abstract"
  )
})


# ── survey_taylor — happy paths ────────────────────────────────────────────────

test_that("survey_taylor() creates valid object for simple random sample", {
  df <- data.frame(y = rnorm(20), wt = runif(20, 0.5, 2))
  d <- survey_taylor(
    data = df,
    variables = .taylor_vars(weights = "wt")
  )
  test_invariants(d)
  expect_true(S7::S7_inherits(d, survey_taylor))
  expect_identical(d@variables$weights, "wt")
  expect_identical(d@variables$ids, NULL)
  expect_identical(d@variables$strata, NULL)
  expect_identical(d@variables$fpc, NULL)
  expect_false(d@variables$nest)
  expect_false(d@variables$probs_provided)
})

test_that("survey_taylor() creates valid object for stratified cluster design", {
  set.seed(42)
  df <- .df10()
  d <- survey_taylor(
    data = df,
    variables = .taylor_vars(
      weights = "wt",
      ids = "psu",
      strata = "strata",
      fpc = "fpc",
      nest = TRUE
    )
  )
  test_invariants(d)
  expect_true(S7::S7_inherits(d, survey_taylor))
  expect_identical(d@variables$ids, "psu")
  expect_identical(d@variables$strata, "strata")
  expect_identical(d@variables$fpc, "fpc")
  expect_true(d@variables$nest)
})

test_that("survey_taylor() creates valid object for two-stage cluster design", {
  df <- data.frame(
    psu = paste0("p", rep(1:5, 4)),
    ssu = paste0("s", rep(1:4, 5)),
    wt = runif(20, 1, 3),
    y = rnorm(20),
    stringsAsFactors = FALSE
  )
  d <- survey_taylor(
    data = df,
    variables = .taylor_vars(
      weights = "wt",
      ids = c("psu", "ssu")
    )
  )
  test_invariants(d)
  expect_identical(d@variables$ids, c("psu", "ssu"))
})

test_that("survey_taylor() allows NA weights (non-NA must be positive)", {
  df <- data.frame(
    wt = c(1.0, NA_real_, 2.0, 0.5),
    y = 1:4
  )
  d <- survey_taylor(
    data = df,
    variables = .taylor_vars(weights = "wt")
  )
  test_invariants(d)
  expect_true(is.na(d@data$wt[2]))
})

test_that("survey_taylor() has @groups = character(0) in Phase 0", {
  df <- data.frame(y = 1:5, wt = rep(1, 5))
  d <- survey_taylor(
    data = df,
    variables = .taylor_vars(weights = "wt")
  )
  expect_identical(d@groups, character(0))
})

test_that("survey_taylor() inherits from survey_base", {
  df <- data.frame(y = 1:5, wt = rep(1, 5))
  d <- survey_taylor(
    data = df,
    variables = .taylor_vars(weights = "wt")
  )
  expect_true(S7::S7_inherits(d, survey_base))
})

test_that("survey_taylor() stores empty survey_metadata by default", {
  df <- data.frame(y = 1:5, wt = rep(1, 5))
  d <- survey_taylor(
    data = df,
    variables = .taylor_vars(weights = "wt")
  )
  expect_true(S7::S7_inherits(d@metadata, survey_metadata))
  expect_identical(d@metadata@variable_labels, list())
})


# ── survey_taylor — validator error paths (errors 31–35) ──────────────────────

test_that("survey_taylor validator: error 31 — design var not in data", {
  df <- data.frame(y = 1:5, wt = rep(1, 5))
  expect_error(
    survey_taylor(
      data = df,
      variables = .taylor_vars(weights = "wt", ids = "nonexistent")
    ),
    class = "surveycore_error_design_var_missing"
  )
})

test_that("survey_taylor validator: error 31 — weight col not in data", {
  df <- data.frame(y = 1:5)
  expect_error(
    survey_taylor(
      data = df,
      variables = .taylor_vars(weights = "missing_wt")
    ),
    class = "surveycore_error_design_var_missing"
  )
})

test_that("survey_taylor validator: error 34 — ids is a list-column", {
  df <- data.frame(wt = rep(1, 3), y = 1:3, stringsAsFactors = FALSE)
  df$lc <- list(1, 2, 3) # list-column
  expect_error(
    survey_taylor(
      data = df,
      variables = .taylor_vars(weights = "wt", ids = "lc")
    ),
    class = "surveycore_error_design_var_list"
  )
})

test_that("survey_taylor validator: error 32 — non-numeric weight column", {
  df <- data.frame(
    wt = c("a", "b", "c"),
    y = 1:3,
    stringsAsFactors = FALSE
  )
  expect_error(
    survey_taylor(
      data = df,
      variables = .taylor_vars(weights = "wt")
    ),
    class = "surveycore_error_weights_not_numeric"
  )
})

test_that("survey_taylor validator: error 33 — weight of zero", {
  df <- data.frame(wt = c(1, 0, 1), y = 1:3)
  expect_error(
    survey_taylor(
      data = df,
      variables = .taylor_vars(weights = "wt")
    ),
    class = "surveycore_error_weights_nonpositive"
  )
})

test_that("survey_taylor validator: error 33 — negative weight", {
  df <- data.frame(wt = c(1, -0.5, 1), y = 1:3)
  expect_error(
    survey_taylor(
      data = df,
      variables = .taylor_vars(weights = "wt")
    ),
    class = "surveycore_error_weights_nonpositive"
  )
})

test_that("survey_taylor validator: warning 35 — PSU crossing strata", {
  # psu_1 appears in both strata s1 and s2
  df <- data.frame(
    wt = rep(1, 4),
    psu = c("psu_1", "psu_1", "psu_2", "psu_2"),
    strata = c("s1", "s2", "s1", "s2"),
    y = 1:4,
    stringsAsFactors = FALSE
  )
  expect_warning(
    survey_taylor(
      data = df,
      variables = .taylor_vars(weights = "wt", ids = "psu", strata = "strata")
    ),
    class = "surveycore_warning_psu_multi_strata"
  )
})

test_that("survey_taylor validator: no PSU warning when nest = TRUE", {
  df <- data.frame(
    wt = rep(1, 4),
    psu = c("psu_1", "psu_1", "psu_2", "psu_2"),
    strata = c("s1", "s2", "s1", "s2"),
    y = 1:4,
    stringsAsFactors = FALSE
  )
  # Should NOT warn when nest = TRUE
  expect_no_warning(
    survey_taylor(
      data = df,
      variables = .taylor_vars(
        weights = "wt",
        ids = "psu",
        strata = "strata",
        nest = TRUE
      )
    )
  )
})


# ── survey_replicate — happy paths ─────────────────────────────────────────────

test_that("survey_replicate() creates valid BRR design", {
  set.seed(1)
  df <- .df_rep(R = 4L)
  d <- survey_replicate(
    data = df,
    variables = .rep_vars(
      weights = "wt",
      repweights = paste0("rw", 1:4),
      type = "BRR"
    )
  )
  test_invariants(d)
  expect_true(S7::S7_inherits(d, survey_replicate))
  expect_identical(d@variables$weights, "wt")
  expect_identical(d@variables$repweights, paste0("rw", 1:4))
  expect_identical(d@variables$type, "BRR")
})

test_that("survey_replicate() creates valid JK1 design", {
  set.seed(2)
  df <- .df_rep(R = 10L)
  d <- survey_replicate(
    data = df,
    variables = .rep_vars(
      weights = "wt",
      repweights = paste0("rw", 1:10),
      type = "JK1"
    )
  )
  test_invariants(d)
  expect_identical(d@variables$type, "JK1")
})

test_that("survey_replicate() inherits from survey_base", {
  df <- .df_rep(R = 2L)
  d <- survey_replicate(
    data = df,
    variables = .rep_vars(
      weights = "wt",
      repweights = c("rw1", "rw2"),
      type = "bootstrap"
    )
  )
  expect_true(S7::S7_inherits(d, survey_base))
})


# ── survey_replicate — validator error paths ───────────────────────────────────

test_that("survey_replicate validator: missing weight column", {
  df <- data.frame(rw1 = 1:3, rw2 = 1:3, y = 1:3)
  expect_error(
    survey_replicate(
      data = df,
      variables = .rep_vars(
        weights = "wt",
        repweights = c("rw1", "rw2")
      )
    ),
    class = "surveycore_error_design_var_missing"
  )
})

test_that("survey_replicate validator: missing repweight column", {
  df <- data.frame(wt = rep(1, 3), rw1 = 1:3, y = 1:3)
  expect_error(
    survey_replicate(
      data = df,
      variables = .rep_vars(
        weights = "wt",
        repweights = c("rw1", "rw_missing")
      )
    ),
    class = "surveycore_error_design_var_missing"
  )
})

test_that("survey_replicate validator: non-positive weight", {
  df <- data.frame(wt = c(1, 0, 1), rw1 = c(1, 1, 1), y = 1:3)
  expect_error(
    survey_replicate(
      data = df,
      variables = .rep_vars(
        weights = "wt",
        repweights = "rw1"
      )
    ),
    class = "surveycore_error_weights_nonpositive"
  )
})

test_that("survey_replicate validator: non-numeric repweight column", {
  df <- data.frame(
    wt = rep(1, 3),
    rw1 = c("a", "b", "c"),
    y = 1:3,
    stringsAsFactors = FALSE
  )
  expect_error(
    survey_replicate(
      data = df,
      variables = .rep_vars(
        weights = "wt",
        repweights = "rw1"
      )
    ),
    class = "surveycore_error_repweights_not_numeric"
  )
})


# ── survey_twophase — happy paths ──────────────────────────────────────────────

test_that("survey_twophase() creates valid object with minimal spec", {
  df <- data.frame(
    wt = runif(20, 0.5, 2),
    psu = paste0("p", rep(1:5, 4)),
    ph2 = c(rep(TRUE, 10), rep(FALSE, 10)),
    y = rnorm(20),
    stringsAsFactors = FALSE
  )
  d <- survey_twophase(
    data = df,
    variables = list(
      phase1 = .taylor_vars(weights = "wt", ids = "psu"),
      phase2 = list(ids = NULL, strata = NULL, probs = NULL, fpc = NULL),
      subset = "ph2",
      method = "full",
      visible_vars = NULL
    )
  )
  test_invariants(d)
  expect_true(S7::S7_inherits(d, survey_twophase))
  expect_identical(d@variables$subset, "ph2")
  expect_identical(d@variables$method, "full")
})

test_that("survey_twophase() creates valid object with phase2 strata col", {
  df <- data.frame(
    wt = runif(20, 0.5, 2),
    ph2 = c(rep(TRUE, 10), rep(FALSE, 10)),
    ph2_str = c(rep(c("A", "B"), 5), rep(NA, 10)),
    y = rnorm(20),
    stringsAsFactors = FALSE
  )
  d <- survey_twophase(
    data = df,
    variables = list(
      phase1 = .taylor_vars(weights = "wt"),
      phase2 = list(
        ids = NULL,
        strata = "ph2_str",
        probs = NULL,
        fpc = NULL
      ),
      subset = "ph2",
      method = "approx",
      visible_vars = NULL
    )
  )
  test_invariants(d)
  expect_identical(d@variables$phase2$strata, "ph2_str")
})

test_that("survey_twophase() inherits from survey_base", {
  df <- data.frame(
    wt = rep(1, 10),
    ph2 = c(rep(TRUE, 5), rep(FALSE, 5)),
    y = rnorm(10)
  )
  d <- survey_twophase(
    data = df,
    variables = list(
      phase1 = .taylor_vars(weights = "wt"),
      phase2 = list(ids = NULL, strata = NULL, probs = NULL, fpc = NULL),
      subset = "ph2",
      method = "simple"
    )
  )
  expect_true(S7::S7_inherits(d, survey_base))
})


# ── survey_twophase — validator error paths ────────────────────────────────────

test_that("survey_twophase validator: subset column not in data", {
  df <- data.frame(wt = rep(1, 5), y = 1:5)
  expect_error(
    survey_twophase(
      data = df,
      variables = list(
        phase1 = .taylor_vars(weights = "wt"),
        phase2 = list(ids = NULL, strata = NULL, probs = NULL, fpc = NULL),
        subset = "ph2_missing",
        method = "full"
      )
    ),
    class = "surveycore_error_design_var_missing"
  )
})

test_that("survey_twophase validator: subset column not logical", {
  df <- data.frame(wt = rep(1, 5), ph2 = 1:5, y = 1:5)
  expect_error(
    survey_twophase(
      data = df,
      variables = list(
        phase1 = .taylor_vars(weights = "wt"),
        phase2 = list(ids = NULL, strata = NULL, probs = NULL, fpc = NULL),
        subset = "ph2",
        method = "full"
      )
    ),
    class = "surveycore_error_subset_not_logical"
  )
})

test_that("survey_twophase validator: phase2 design col not in data", {
  df <- data.frame(
    wt = rep(1, 10),
    ph2 = c(rep(TRUE, 5), rep(FALSE, 5)),
    y = 1:10
  )
  expect_error(
    survey_twophase(
      data = df,
      variables = list(
        phase1 = .taylor_vars(weights = "wt"),
        phase2 = list(
          ids = NULL,
          strata = "missing_strata_col",
          probs = NULL,
          fpc = NULL
        ),
        subset = "ph2",
        method = "full"
      )
    ),
    class = "surveycore_error_design_var_missing"
  )
})

test_that("survey_twophase validator: warning 26 — phase2 col all-NA in ph2", {
  df <- data.frame(
    wt = rep(1, 10),
    ph2 = c(rep(TRUE, 5), rep(FALSE, 5)),
    ph2_str = rep(NA_character_, 10), # all NA within phase 2 rows
    y = 1:10,
    stringsAsFactors = FALSE
  )
  expect_warning(
    survey_twophase(
      data = df,
      variables = list(
        phase1 = .taylor_vars(weights = "wt"),
        phase2 = list(
          ids = NULL,
          strata = "ph2_str",
          probs = NULL,
          fpc = NULL
        ),
        subset = "ph2",
        method = "full"
      )
    ),
    class = "surveycore_warning_phase2_all_na"
  )
})


# ── Cross-class structural tests ───────────────────────────────────────────────

test_that("survey_taylor and survey_replicate are different classes", {
  df_t <- data.frame(wt = rep(1, 5), y = 1:5)
  d_t <- survey_taylor(
    data = df_t,
    variables = .taylor_vars(weights = "wt")
  )
  df_r <- .df_rep(R = 2L)
  d_r <- survey_replicate(
    data = df_r,
    variables = .rep_vars(
      weights = "wt",
      repweights = c("rw1", "rw2")
    )
  )
  expect_false(S7::S7_inherits(d_t, survey_replicate))
  expect_false(S7::S7_inherits(d_r, survey_taylor))
})

test_that("all three concrete classes inherit from survey_base", {
  df_t <- data.frame(wt = rep(1, 5), y = 1:5)
  df_r <- .df_rep(R = 2L)
  df_tp <- data.frame(
    wt = rep(1, 10),
    ph2 = c(rep(TRUE, 5), rep(FALSE, 5)),
    y = 1:10
  )

  d_t <- survey_taylor(
    data = df_t,
    variables = .taylor_vars(weights = "wt")
  )
  d_r <- survey_replicate(
    data = df_r,
    variables = .rep_vars(weights = "wt", repweights = c("rw1", "rw2"))
  )
  d_tp <- survey_twophase(
    data = df_tp,
    variables = list(
      phase1 = .taylor_vars(weights = "wt"),
      phase2 = list(ids = NULL, strata = NULL, probs = NULL, fpc = NULL),
      subset = "ph2",
      method = "full"
    )
  )

  expect_true(S7::S7_inherits(d_t, survey_base))
  expect_true(S7::S7_inherits(d_r, survey_base))
  expect_true(S7::S7_inherits(d_tp, survey_base))
})

# ── survey_taylor validator: list-column design variable ──────────────────────

test_that("survey_taylor validator rejects list-column design variable", {
  df <- data.frame(
    wt = rep(1, 5),
    y = 1:5,
    stringsAsFactors = FALSE
  )
  # Add a list-column for the weights position
  df$psu <- vector("list", 5) # list-column
  for (i in seq_len(5)) {
    df$psu[[i]] <- i
  }

  expect_error(
    survey_taylor(
      data = df,
      variables = .taylor_vars(weights = "wt", ids = "psu")
    ),
    class = "surveycore_error_design_var_list"
  )
})

test_that("survey_taylor validator rejects non-numeric weight column", {
  df <- data.frame(
    wt = as.character(1:5), # character, not numeric
    y = 1:5,
    stringsAsFactors = FALSE
  )
  expect_error(
    survey_taylor(
      data = df,
      variables = .taylor_vars(weights = "wt")
    ),
    class = "surveycore_error_weights_not_numeric"
  )
})

# ── survey_replicate validator: list-column design variable ───────────────────

test_that("survey_replicate validator rejects list-column design variable", {
  df <- .df_rep(R = 2L)
  df$rw1 <- as.list(df$rw1) # make a repweight column a list-column

  expect_error(
    survey_replicate(
      data = df,
      variables = .rep_vars(weights = "wt", repweights = c("rw1", "rw2"))
    ),
    class = "surveycore_error_design_var_list"
  )
})

# ── survey_nonprob validator: non-numeric weight column ───────────────────

test_that("survey_nonprob validator rejects non-numeric weight column", {
  df <- data.frame(y = 1:5, wt = c("a", "b", "c", "d", "e"))

  expect_error(
    survey_nonprob(
      data = df,
      variables = list(
        weights = "wt",
        probs_provided = FALSE,
        ids = NULL,
        strata = NULL,
        fpc = NULL,
        nest = FALSE,
        visible_vars = NULL
      )
    ),
    class = "surveycore_error_weights_not_numeric"
  )
})

# ── survey_nonprob validator: zero weights ────────────────────────────────────

test_that("survey_nonprob allows zero weights after post-construction assignment", {
  df <- data.frame(x = 1:5, w = c(1, 2, 3, 4, 5))
  obj <- as_survey_nonprob(df, weights = w)
  new_data <- obj@data
  new_data$w <- c(1, 0, 0, 4, 0)
  obj@data <- new_data
  test_invariants(obj)
  expect_equal(sum(obj@data$w == 0), 3L)
})

test_that("survey_nonprob validator accepts zero weights with at least one positive", {
  df <- data.frame(x = 1:5, w = c(1, 0, 0, 2, 0))
  obj <- survey_nonprob(
    data = df,
    variables = list(
      weights = "w",
      repweights = NULL,
      type = NULL,
      scale = NULL,
      rscales = NULL,
      mse = NULL,
      probs_provided = FALSE,
      ids = NULL,
      strata = NULL,
      fpc = NULL,
      nest = FALSE,
      visible_vars = NULL
    )
  )
  test_invariants(obj)
  expect_s3_class(obj@data, "data.frame")
  expect_equal(sum(obj@data$w == 0), 3L)
})

test_that("survey_nonprob validator rejects negative weights", {
  df <- data.frame(x = 1:3, w = c(1, -0.5, 2))
  expect_error(
    survey_nonprob(
      data = df,
      variables = list(
        weights = "w",
        probs_provided = FALSE,
        ids = NULL,
        strata = NULL,
        fpc = NULL,
        nest = FALSE,
        visible_vars = NULL
      )
    ),
    class = "surveycore_error_weights_negative"
  )
})

test_that("survey_nonprob validator rejects all-zero weights", {
  df <- data.frame(x = 1:3, w = c(0, 0, 0))
  expect_error(
    survey_nonprob(
      data = df,
      variables = list(
        weights = "w",
        probs_provided = FALSE,
        ids = NULL,
        strata = NULL,
        fpc = NULL,
        nest = FALSE,
        visible_vars = NULL
      )
    ),
    class = "surveycore_error_weights_all_zero"
  )
})

test_that("survey_nonprob validator accepts single positive weight among zeros", {
  df <- data.frame(x = 1:5, w = c(0, 0, 0, 0, 0.001))
  obj <- survey_nonprob(
    data = df,
    variables = list(
      weights = "w",
      repweights = NULL,
      type = NULL,
      scale = NULL,
      rscales = NULL,
      mse = NULL,
      probs_provided = FALSE,
      ids = NULL,
      strata = NULL,
      fpc = NULL,
      nest = FALSE,
      visible_vars = NULL
    )
  )
  test_invariants(obj)
})

test_that("survey_nonprob validator accepts mix of zeros and NAs with one positive", {
  df <- data.frame(x = 1:4, w = c(0, NA, 1, 0))
  obj <- survey_nonprob(
    data = df,
    variables = list(
      weights = "w",
      repweights = NULL,
      type = NULL,
      scale = NULL,
      rscales = NULL,
      mse = NULL,
      probs_provided = FALSE,
      ids = NULL,
      strata = NULL,
      fpc = NULL,
      nest = FALSE,
      visible_vars = NULL
    )
  )
  test_invariants(obj)
})

test_that("survey_nonprob validator rejects mix of zeros and negatives", {
  df <- data.frame(x = 1:3, w = c(0, -1, 0))
  expect_error(
    survey_nonprob(
      data = df,
      variables = list(
        weights = "w",
        probs_provided = FALSE,
        ids = NULL,
        strata = NULL,
        fpc = NULL,
        nest = FALSE,
        visible_vars = NULL
      )
    ),
    class = "surveycore_error_weights_negative"
  )
})

test_that("survey_nonprob validator rejects all-zero weights with NAs", {
  df <- data.frame(x = 1:3, w = c(0, NA, 0))
  expect_error(
    survey_nonprob(
      data = df,
      variables = list(
        weights = "w",
        probs_provided = FALSE,
        ids = NULL,
        strata = NULL,
        fpc = NULL,
        nest = FALSE,
        visible_vars = NULL
      )
    ),
    class = "surveycore_error_weights_all_zero"
  )
})

test_that("test_invariants() passes for survey_nonprob with zero weights", {
  df <- data.frame(x = 1:5, w = c(1, 2, 3, 4, 5))
  obj <- as_survey_nonprob(df, weights = w)
  new_data <- obj@data
  new_data$w <- c(1, 0, 0, 2, 0)
  obj@data <- new_data
  expect_no_error(test_invariants(obj))
})


# ── survey_metadata sata property ─────────────────────────────────────────────

test_that("survey_metadata has a sata property with list() default", {
  m <- survey_metadata()
  expect_true(S7::S7_inherits(m, survey_metadata))
  expect_identical(m@sata, list())
  expect_identical(length(m@sata), 0L)
})

test_that("survey_metadata sata accepts a named list", {
  m <- survey_metadata(sata = list(news_tv = TRUE, news_online = TRUE))
  expect_identical(m@sata$news_tv, TRUE)
})


# ── survey_taylor @calibration property ────────────────────────────────────────

test_that("survey_taylor has @calibration == NULL by default", {
  df <- make_survey_data(seed = 1)
  design <- as_survey(df, ids = psu, weights = wt, strata = strata)
  test_invariants(design)
  expect_null(design@calibration)
})

test_that("survey_taylor @calibration accepts a list", {
  df <- make_survey_data(seed = 1)
  design <- as_survey(df, ids = psu, weights = wt, strata = strata)
  cd <- as_caldata(df$wt, rep(1.05, nrow(df)), matrix(1, nrow(df), 1))
  design@calibration <- list(cd)
  test_invariants(design)
  expect_type(design@calibration, "list")
  expect_length(design@calibration, 1L)
})

test_that("survey_taylor S7 validator passes when @calibration is NULL", {
  df <- make_survey_data(seed = 1)
  design <- as_survey(df, ids = psu, weights = wt, strata = strata)
  expect_no_error(S7::check_is_S7(design))
})

test_that("survey_taylor S7 validator passes when @calibration is a non-empty list", {
  df <- make_survey_data(seed = 1)
  design <- as_survey(df, ids = psu, weights = wt, strata = strata)
  cd <- as_caldata(df$wt, rep(1.05, nrow(df)), matrix(1, nrow(df), 1))
  design@calibration <- list(cd)
  expect_no_error(S7::check_is_S7(design))
})


# ── survey_replicate @calibration property ─────────────────────────────────────

test_that("survey_replicate has @calibration == NULL by default", {
  df <- make_survey_data(design = "replicate", seed = 1)
  design <- as_survey_replicate(
    df,
    weights = wt,
    repweights = starts_with("repwt_"),
    type = "BRR"
  )
  test_invariants(design)
  expect_null(design@calibration)
})

test_that("survey_replicate @calibration accepts a list", {
  df <- make_survey_data(design = "replicate", seed = 1)
  design <- as_survey_replicate(
    df,
    weights = wt,
    repweights = starts_with("repwt_"),
    type = "BRR"
  )
  cd <- as_caldata(df$wt, rep(1.05, nrow(df)), matrix(1, nrow(df), 1))
  design@calibration <- list(cd)
  test_invariants(design)
  expect_type(design@calibration, "list")
  expect_length(design@calibration, 1L)
})

test_that("survey_replicate S7 validator passes when @calibration is NULL", {
  df <- make_survey_data(design = "replicate", seed = 1)
  design <- as_survey_replicate(
    df,
    weights = wt,
    repweights = starts_with("repwt_"),
    type = "BRR"
  )
  expect_no_error(S7::check_is_S7(design))
})

test_that("survey_replicate S7 validator passes when @calibration is a non-empty list", {
  df <- make_survey_data(design = "replicate", seed = 1)
  design <- as_survey_replicate(
    df,
    weights = wt,
    repweights = starts_with("repwt_"),
    type = "BRR"
  )
  cd <- as_caldata(df$wt, rep(1.05, nrow(df)), matrix(1, nrow(df), 1))
  design@calibration <- list(cd)
  expect_no_error(S7::check_is_S7(design))
})
