# tests/testthat/test-collection-groups.R
#
# Tests for the survey_collection uniform-grouping invariant. Covers:
#   - Internal helpers .check_groups_match() and .propagate_or_match()
#   - @groups property and three validator invariants (G1, G1b, G1c)
#   - as_survey_collection() `group =` argument (G2, G3, G8)
#   - add_survey() propagation (Decision 2 matrix; G4; atomicity)
#   - remove_survey() preserves collection-level grouping
#   - Print snapshot for grouped collection
#   - Dispatch regression with uniform @groups (Decisions 1 and 5)
#
# `[[<-` is intentionally NOT offered — `S7::method("[[<-", ...)` registers
# the method in S3methods metadata with the function object (rather than a
# name) in the method column, which trips R CMD check's `checkReplaceFuns`
# coercion path and produces a spurious WARNING. Use `add_survey()` /
# `remove_survey()` instead.
#
# Layer 1 errors (S7 validator: G1, G1b, G1c): class= only, no snapshot.
# Layer 3 errors (constructors, mutators: G2, G3, G4): dual pattern
# (class= + snapshot).
# G8 warning: expect_warning(class =) + snapshot.


# ── Helpers ────────────────────────────────────────────────────────────────────

# Build an ungrouped survey_taylor from make_survey_data().
.mk_design <- function(seed = 11L, n = 40L, n_psu = 8L, n_strata = 2L) {
  df <- make_survey_data(
    n = n,
    n_psu = n_psu,
    n_strata = n_strata,
    seed = seed
  )
  suppressMessages(suppressWarnings(
    as_survey(df, ids = psu, weights = wt, strata = strata, fpc = fpc)
  ))
}

# Build a design and manually set @groups for direct-construction tests. Does
# NOT go through as_survey_collection() — used to bypass the constructor and
# hit the S7 validator directly.
.mk_design_grouped <- function(groups, seed = 11L) {
  d <- .mk_design(seed = seed)
  d@groups <- groups
  d
}


# ── §0 Internal helpers ────────────────────────────────────────────────────────

test_that(".check_groups_match() returns invisible(TRUE) on identical groups", {
  expect_identical(
    surveycore:::.check_groups_match(
      candidate_groups = c("region", "stratum"),
      target_groups = c("region", "stratum"),
      error_class = "surveycore_error_collection_groups_invariant"
    ),
    TRUE
  )
})

test_that(".check_groups_match() treats character(0) == character(0) as match", {
  expect_identical(
    surveycore:::.check_groups_match(
      candidate_groups = character(0),
      target_groups = character(0),
      error_class = "surveycore_error_collection_groups_invariant"
    ),
    TRUE
  )
})

test_that(".check_groups_match() errors with supplied class on empty mismatch", {
  expect_error(
    surveycore:::.check_groups_match(
      candidate_groups = character(0),
      target_groups = "region",
      error_class = "surveycore_error_collection_groups_invariant"
    ),
    class = "surveycore_error_collection_groups_invariant"
  )
})

test_that(".check_groups_match() is order-sensitive", {
  expect_error(
    surveycore:::.check_groups_match(
      candidate_groups = c("b", "a"),
      target_groups = c("a", "b"),
      error_class = "surveycore_error_collection_groups_invariant"
    ),
    class = "surveycore_error_collection_groups_invariant"
  )
})

test_that(".propagate_or_match() returns target when candidate is empty", {
  expect_identical(
    surveycore:::.propagate_or_match(
      candidate_groups = character(0),
      target_groups = c("region", "stratum"),
      name = "wave1",
      error_class = "surveycore_error_collection_group_conflict"
    ),
    c("region", "stratum")
  )
})

test_that(".propagate_or_match() returns candidate when identical to target", {
  expect_identical(
    surveycore:::.propagate_or_match(
      candidate_groups = c("region", "stratum"),
      target_groups = c("region", "stratum"),
      name = "wave1",
      error_class = "surveycore_error_collection_group_conflict"
    ),
    c("region", "stratum")
  )
})

test_that(".propagate_or_match() errors on non-empty mismatch", {
  expect_error(
    surveycore:::.propagate_or_match(
      candidate_groups = c("region"),
      target_groups = c("stratum"),
      name = "wave1",
      error_class = "surveycore_error_collection_group_conflict"
    ),
    class = "surveycore_error_collection_group_conflict"
  )
})


# ── §1 Validator invariants (Layer 1 — class= only) ────────────────────────────

test_that("validator rejects member with non-empty @groups when coll@groups empty", {
  d_grouped <- .mk_design_grouped(groups = "region")
  expect_error(
    survey_collection(surveys = list(a = d_grouped), groups = character(0)),
    class = "surveycore_error_collection_groups_invariant"
  )
})

test_that("validator rejects mismatched member @groups (names first divergent)", {
  d1 <- .mk_design_grouped(groups = "region", seed = 11L)
  d2 <- .mk_design_grouped(groups = "stratum", seed = 12L)
  expect_error(
    survey_collection(
      surveys = list(wave1 = d1, wave2 = d2),
      groups = "region"
    ),
    class = "surveycore_error_collection_groups_invariant",
    regexp = "wave2"
  )
})

test_that("validator is order-sensitive", {
  d1 <- .mk_design_grouped(groups = c("strata", "psu"), seed = 11L)
  expect_error(
    survey_collection(
      surveys = list(wave1 = d1),
      groups = c("psu", "strata")
    ),
    class = "surveycore_error_collection_groups_invariant"
  )
})

test_that("validator rejects @groups naming a column missing from member's @data (G1b)", {
  # Member's @groups matches coll@groups but the named column isn't in @data.
  d <- .mk_design(seed = 11L)
  # Use a non-design column name so assigning @groups won't conflict with
  # design validator on @data checks.
  d@groups <- "not_in_data_col"
  expect_error(
    survey_collection(
      surveys = list(wave1 = d),
      groups = "not_in_data_col"
    ),
    class = "surveycore_error_collection_group_not_in_member_data"
  )
})

test_that("validator rejects @groups with NA (G1c)", {
  # Direct S7 construction with an empty surveys list would hit C1 first;
  # supply one member so validator reaches Invariant 3.
  d <- .mk_design_grouped(groups = c("strata", NA_character_))
  expect_error(
    survey_collection(
      surveys = list(wave1 = d),
      groups = c("strata", NA_character_)
    ),
    class = "surveycore_error_collection_groups_malformed"
  )
})

test_that("validator rejects @groups with empty string (G1c)", {
  d <- .mk_design_grouped(groups = c("strata", ""))
  expect_error(
    survey_collection(
      surveys = list(wave1 = d),
      groups = c("strata", "")
    ),
    class = "surveycore_error_collection_groups_malformed"
  )
})

test_that("validator rejects duplicated @groups entries (G1c)", {
  d <- .mk_design_grouped(groups = c("strata", "strata"))
  expect_error(
    survey_collection(
      surveys = list(wave1 = d),
      groups = c("strata", "strata")
    ),
    class = "surveycore_error_collection_groups_malformed"
  )
})

test_that("validator reports all G1c conditions in a multi-violation vector", {
  bad <- c(NA_character_, "", "a", "a")
  d <- .mk_design_grouped(groups = bad)
  err <- expect_error(
    survey_collection(
      surveys = list(wave1 = d),
      groups = bad
    ),
    class = "surveycore_error_collection_groups_malformed"
  )
  # The message should mention NA, empty-string, and duplicates in one error.
  msg <- conditionMessage(err)
  expect_true(grepl("NA", msg, fixed = TRUE))
  expect_true(grepl("empty", msg))
  expect_true(grepl("uplicate", msg))
})

test_that("validator accepts collections with empty @groups (backwards compatible)", {
  d1 <- .mk_design(seed = 11L)
  d2 <- .mk_design(seed = 12L)
  coll <- survey_collection(surveys = list(wave1 = d1, wave2 = d2))
  expect_identical(coll@groups, character(0))
  test_collection_groups_invariant(coll)
})


# ── §2 as_survey_collection() — happy paths ───────────────────────────────────

test_that("as_survey_collection() with group = propagates to all members", {
  d1 <- .mk_design(seed = 11L)
  d2 <- .mk_design(seed = 12L)
  coll <- as_survey_collection(wave1 = d1, wave2 = d2, group = strata)
  expect_identical(coll@groups, "strata")
  test_collection_groups_invariant(coll)
})

test_that("as_survey_collection() with group = c(...) sets multi-var groups", {
  d1 <- .mk_design(seed = 11L)
  d2 <- .mk_design(seed = 12L)
  coll <- as_survey_collection(
    wave1 = d1,
    wave2 = d2,
    group = c(strata, psu)
  )
  expect_identical(coll@groups, c("strata", "psu"))
  test_collection_groups_invariant(coll)
})

test_that("as_survey_collection() adopts uniform non-empty member @groups", {
  d1 <- .mk_design_grouped(groups = "strata", seed = 11L)
  d2 <- .mk_design_grouped(groups = "strata", seed = 12L)
  coll <- as_survey_collection(wave1 = d1, wave2 = d2)
  expect_identical(coll@groups, "strata")
  test_collection_groups_invariant(coll)
})

test_that("as_survey_collection() on ungrouped members yields character(0)", {
  d1 <- .mk_design(seed = 11L)
  d2 <- .mk_design(seed = 12L)
  coll <- as_survey_collection(wave1 = d1, wave2 = d2)
  expect_identical(coll@groups, character(0))
  test_collection_groups_invariant(coll)
})

test_that("empty group = forms collapse to adopt-from-members branch", {
  d1 <- .mk_design(seed = 11L)
  d2 <- .mk_design(seed = 12L)

  coll_null <- as_survey_collection(wave1 = d1, wave2 = d2, group = NULL)
  expect_identical(coll_null@groups, character(0))

  coll_ch0 <- as_survey_collection(
    wave1 = d1,
    wave2 = d2,
    group = character(0)
  )
  expect_identical(coll_ch0@groups, character(0))

  coll_c <- as_survey_collection(wave1 = d1, wave2 = d2, group = c())
  expect_identical(coll_c@groups, character(0))

  coll_all <- as_survey_collection(
    wave1 = d1,
    wave2 = d2,
    group = tidyselect::all_of(character(0))
  )
  expect_identical(coll_all@groups, character(0))
})


# ── §2 as_survey_collection() — G8 override warning ───────────────────────────

test_that("group = supplied overrides divergent members with G8 warning", {
  d1 <- .mk_design_grouped(groups = "psu", seed = 11L)
  d2 <- .mk_design_grouped(groups = "psu", seed = 12L)
  # Two divergent members → two G8 warnings (one per member). Collect all
  # via withCallingHandlers to confirm both fire.
  warnings_raised <- character(0)
  withCallingHandlers(
    coll <- as_survey_collection(
      wave1 = d1,
      wave2 = d2,
      group = strata
    ),
    surveycore_warning_collection_group_overridden = function(w) {
      warnings_raised <<- c(warnings_raised, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  expect_length(warnings_raised, 2L)
  expect_identical(coll@groups, "strata")
  expect_identical(coll@surveys$wave1@groups, "strata")
  expect_identical(coll@surveys$wave2@groups, "strata")
})

test_that("group = supplied emits G8 only for non-empty divergent members", {
  d_empty <- .mk_design(seed = 11L) # character(0)
  d_divergent <- .mk_design_grouped(groups = "psu", seed = 12L)
  # Warnings: exactly one G8 for d_divergent; d_empty silently propagated.
  warnings_raised <- character(0)
  withCallingHandlers(
    coll <- as_survey_collection(
      wave1 = d_empty,
      wave2 = d_divergent,
      group = strata
    ),
    surveycore_warning_collection_group_overridden = function(w) {
      warnings_raised <<- c(warnings_raised, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  expect_length(warnings_raised, 1L)
  expect_true(grepl("wave2", warnings_raised[[1L]]))
  expect_identical(coll@groups, "strata")
})

test_that("snapshot: G8 override warning", {
  d1 <- .mk_design_grouped(groups = "psu", seed = 11L)
  expect_snapshot(
    invisible(as_survey_collection(wave1 = d1, group = strata))
  )
})


# ── §2 as_survey_collection() — error paths ───────────────────────────────────

test_that("divergent member @groups with no group = errors G2", {
  d1 <- .mk_design_grouped(groups = "strata", seed = 11L)
  d2 <- .mk_design_grouped(groups = "psu", seed = 12L)
  expect_error(
    as_survey_collection(wave1 = d1, wave2 = d2),
    class = "surveycore_error_collection_group_divergent"
  )
  expect_snapshot(
    error = TRUE,
    as_survey_collection(wave1 = d1, wave2 = d2)
  )
})

test_that("group = naming a column missing from a member errors G3", {
  d1 <- .mk_design(seed = 11L)
  d2 <- .mk_design(seed = 12L)
  # Add a non-design column `region` to only d1. group = region will resolve
  # on d1 but be missing from d2, triggering G3.
  d1@data$region <- rep(c("N", "S"), length.out = nrow(d1@data))
  expect_error(
    as_survey_collection(wave1 = d1, wave2 = d2, group = region),
    class = "surveycore_error_collection_group_var_not_found"
  )
  expect_snapshot(
    error = TRUE,
    as_survey_collection(wave1 = d1, wave2 = d2, group = region)
  )
})

test_that("empty ... still errors C1", {
  expect_error(
    as_survey_collection(),
    class = "surveycore_error_collection_empty"
  )
})


# ── §3 add_survey() — Decision 2 matrix ────────────────────────────────────────

test_that("ungrouped coll + ungrouped new: insert unchanged", {
  d1 <- .mk_design(seed = 11L)
  d2 <- .mk_design(seed = 12L)
  coll <- as_survey_collection(wave1 = d1)
  coll2 <- add_survey(coll, wave2 = d2)
  expect_identical(coll2@groups, character(0))
  expect_identical(coll2@surveys$wave2@groups, character(0))
  test_collection_groups_invariant(coll2)
})

test_that("ungrouped coll + grouped new: errors G4", {
  d1 <- .mk_design(seed = 11L)
  d2_grouped <- .mk_design_grouped(groups = "strata", seed = 12L)
  coll <- as_survey_collection(wave1 = d1)
  expect_error(
    add_survey(coll, wave2 = d2_grouped),
    class = "surveycore_error_collection_group_conflict"
  )
  expect_snapshot(
    error = TRUE,
    add_survey(coll, wave2 = d2_grouped)
  )
})

test_that("grouped coll + ungrouped new: propagates groups onto new member", {
  d1 <- .mk_design(seed = 11L)
  d2 <- .mk_design(seed = 12L)
  coll <- as_survey_collection(wave1 = d1, group = strata)
  coll2 <- add_survey(coll, wave2 = d2)
  expect_identical(coll2@groups, "strata")
  expect_identical(coll2@surveys$wave2@groups, "strata")
  test_collection_groups_invariant(coll2)
})

test_that("grouped coll + matching grouped new: insert unchanged", {
  d1 <- .mk_design(seed = 11L)
  d2_match <- .mk_design_grouped(groups = "strata", seed = 12L)
  coll <- as_survey_collection(wave1 = d1, group = strata)
  coll2 <- add_survey(coll, wave2 = d2_match)
  expect_identical(coll2@groups, "strata")
  test_collection_groups_invariant(coll2)
})

test_that("grouped coll + divergent grouped new: errors G4", {
  d1 <- .mk_design(seed = 11L)
  d2_bad <- .mk_design_grouped(groups = "psu", seed = 12L)
  coll <- as_survey_collection(wave1 = d1, group = strata)
  expect_error(
    add_survey(coll, wave2 = d2_bad),
    class = "surveycore_error_collection_group_conflict"
  )
})

test_that("add_survey() is atomic: caller coll unchanged on mid-loop error", {
  d1 <- .mk_design(seed = 11L)
  d_ok <- .mk_design(seed = 12L)
  d_bad <- .mk_design_grouped(groups = "psu", seed = 13L)
  d_ok2 <- .mk_design(seed = 14L)

  coll <- as_survey_collection(wave1 = d1, group = strata)
  pre_names <- names(coll@surveys)
  pre_length <- length(coll@surveys)

  tryCatch(
    add_survey(coll, a = d_ok, b = d_bad, c = d_ok2),
    surveycore_error_collection_group_conflict = function(e) NULL
  )

  expect_identical(names(coll@surveys), pre_names)
  expect_identical(length(coll@surveys), pre_length)
})


# ── §3.5 remove_survey() — preserves collection grouping ──────────────────────

test_that("remove_survey() preserves coll@groups and members mirror", {
  d1 <- .mk_design(seed = 11L)
  d2 <- .mk_design(seed = 12L)
  d3 <- .mk_design(seed = 13L)
  coll <- as_survey_collection(a = d1, b = d2, c = d3, group = strata)

  coll2 <- remove_survey(coll, "b")
  expect_identical(coll2@groups, "strata")
  expect_identical(coll2@surveys$a@groups, "strata")
  expect_identical(coll2@surveys$c@groups, "strata")
  test_collection_groups_invariant(coll2)
})

test_that("remove_survey() down to zero errors C1", {
  d1 <- .mk_design(seed = 11L)
  coll <- as_survey_collection(a = d1, group = strata)
  expect_error(
    remove_survey(coll, "a"),
    class = "surveycore_error_collection_empty"
  )
})


# ── G1b fires through propagation paths ────────────────────────────────────────

test_that("add_survey() onto grouped coll with new lacking grouping column errors G1b", {
  # Add a non-design column `region` to d1 so the collection can be grouped
  # by it; d2 lacks the column so propagating "region" onto d2 fails G1b.
  d1 <- .mk_design(seed = 11L)
  d1@data$region <- rep(c("N", "S"), length.out = nrow(d1@data))
  coll <- as_survey_collection(wave1 = d1, group = region)

  d2 <- .mk_design(seed = 12L)
  expect_error(
    add_survey(coll, wave2 = d2),
    class = "surveycore_error_collection_group_not_in_member_data"
  )
})


# ── Dispatch regressions — Decisions 1 + 5 ─────────────────────────────────────

test_that("get_means() on grouped collection returns a single result with group column", {
  d1 <- .mk_design(seed = 11L)
  d2 <- .mk_design(seed = 12L)
  coll <- as_survey_collection(wave1 = d1, wave2 = d2, group = strata)

  res <- suppressWarnings(get_means(coll, y1))
  expect_true(".survey" %in% names(res))
  expect_true("strata" %in% names(res))
  # Group column must be populated (no NAs from shape mismatch)
  expect_false(any(is.na(res$strata)))
})

test_that("get_means() stacks coll@groups with call-site group (column order)", {
  d1 <- .mk_design(seed = 11L)
  d2 <- .mk_design(seed = 12L)
  coll <- as_survey_collection(wave1 = d1, wave2 = d2, group = strata)

  res <- suppressWarnings(get_means(coll, y1, group = psu))
  cols <- names(res)
  strata_pos <- match("strata", cols)
  psu_pos <- match("psu", cols)
  expect_true(!is.na(strata_pos) && !is.na(psu_pos))
  # coll@groups columns come BEFORE call-site group columns
  expect_lt(strata_pos, psu_pos)
  # .survey column should be first
  expect_identical(cols[[1L]], ".survey")
})


# ── Print snapshot ─────────────────────────────────────────────────────────────

test_that("grouped collection print includes Groups: line", {
  d1 <- .mk_design(seed = 11L)
  d2 <- .mk_design(seed = 12L)
  coll <- as_survey_collection(wave1 = d1, wave2 = d2, group = strata)
  expect_snapshot(print(coll))
})

test_that("ungrouped collection print omits Groups: line", {
  d1 <- .mk_design(seed = 11L)
  d2 <- .mk_design(seed = 12L)
  coll <- as_survey_collection(wave1 = d1, wave2 = d2)
  expect_snapshot(print(coll))
})
