# tests/testthat/test-invariants.R
#
# Meta-tests: verify that test_invariants() correctly detects invalid survey
# objects. These confirm the checker actually fires on each invariant violation
# so that constructor tests relying on test_invariants() have meaningful guards.
#
# Technique: construct a valid object, then break exactly one invariant by
# setting the underlying S7 attribute directly via attr() — this bypasses the
# S7 property setter and class validator.
# S7 stores each property as a named attribute (e.g., attr(x, "data")).
#
# Approach: rather than relying on expect_failure() — which only catches
# testthat expectation failures, not R errors — we use a custom helper that
# catches both failure conditions and R errors thrown by test_invariants().


# ── Helpers ───────────────────────────────────────────────────────────────────

# Build a valid survey_taylor to start from.
.make_valid_taylor <- function() {
  df <- make_survey_data(n = 50L, n_psu = 10L, n_strata = 2L, seed = 99L)
  as_survey(df, ids = psu, weights = wt, strata = strata, fpc = fpc, nest = TRUE)
}

# Returns TRUE if test_invariants(design) produces any expectation failure or
# R error. Returns FALSE if it runs completely without issues.
.breaks_invariants <- function(design) {
  found_problem <- FALSE
  tryCatch(
    withCallingHandlers(
      test_invariants(design),
      expectation_failure = function(e) {
        found_problem <<- TRUE
      }
    ),
    error = function(e) {
      found_problem <<- TRUE
    }
  )
  found_problem
}


# ── Invariant: @data is a data.frame ──────────────────────────────────────────

test_that("test_invariants() detects @data set to a non-data.frame", {
  d      <- .make_valid_taylor()
  broken <- d
  attr(broken, "data") <- list(x = 1:5)  # list, not data.frame
  expect_true(.breaks_invariants(broken))
})


# ── Invariant: @data has at least 1 row ───────────────────────────────────────

test_that("test_invariants() detects @data with 0 rows", {
  d      <- .make_valid_taylor()
  broken <- d
  attr(broken, "data") <- d@data[integer(0L), ]
  expect_true(.breaks_invariants(broken))
})


# ── Invariant: @data has no duplicate column names ────────────────────────────

test_that("test_invariants() detects @data with duplicate column names", {
  d      <- .make_valid_taylor()
  broken <- d
  duped  <- d@data
  names(duped)[1:2] <- c("dup", "dup")
  attr(broken, "data") <- duped
  expect_true(.breaks_invariants(broken))
})


# ── Invariant: design columns exist in @data ─────────────────────────────────

test_that("test_invariants() detects missing design column in @data", {
  d      <- .make_valid_taylor()
  broken <- d
  # Drop the weights column from @data (keep @variables pointing to it)
  no_wt <- d@data[, setdiff(names(d@data), d@variables$weights), drop = FALSE]
  attr(broken, "data") <- no_wt
  expect_true(.breaks_invariants(broken))
})


# ── Invariant: weight column is numeric and positive ─────────────────────────

test_that("test_invariants() detects non-positive weight values", {
  d       <- .make_valid_taylor()
  broken  <- d
  bad_dat <- d@data
  bad_dat[[d@variables$weights]][[1L]] <- -1
  attr(broken, "data") <- bad_dat
  expect_true(.breaks_invariants(broken))
})


# ── Invariant: @metadata is a survey_metadata object ─────────────────────────

test_that("test_invariants() detects @metadata set to a non-metadata object", {
  d      <- .make_valid_taylor()
  broken <- d
  attr(broken, "metadata") <- list(not_a_metadata = TRUE)
  expect_true(.breaks_invariants(broken))
})
