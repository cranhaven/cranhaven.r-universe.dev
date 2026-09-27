# tests/testthat/helper-test-data.R
#
# Shared test infrastructure loaded automatically by testthat before any test
# file runs. Provides:
#   - make_survey_data()  — synthetic survey data generator
#   - test_invariants()   — formal invariant checker for survey objects
#
# Dependency note: test_invariants() references S7 class objects
# (survey_replicate, survey_metadata) and .get_design_vars_flat() from
# R/07-utils.R. Both are available after the package is loaded by
# devtools::test() / devtools::load_all().

# ------------------------------------------------------------------------------
# make_survey_data()
# ------------------------------------------------------------------------------

#' Create synthetic survey data for testing
#'
#' Generates realistic but artificial survey data with a known design structure.
#' PSU sizes vary (not equal), weights vary (lognormal), strata sizes are
#' imbalanced. Returns a plain data.frame — NOT a survey object.
#'
#' Column layout:
#'   - psu      : character PSU identifiers (globally unique)
#'   - strata   : character stratum identifiers
#'   - fpc      : integer population size per stratum (for FPC)
#'   - wt       : numeric sampling weights (lognormal, positive)
#'   - y1       : numeric continuous outcome (mean ~50)
#'   - y2       : numeric continuous outcome (mean ~0)
#'   - y3       : integer binary outcome (0/1, ~30% = 1)
#'   - group    : character categorical variable ("A", "B", "C")
#'
#' Additional columns by n_ssu/n_unit:
#'   - n_ssu non-NULL: ssu (character SSU IDs), fpc2 (integer, n_ssu * 2L)
#'   - n_unit non-NULL: unit (character unit IDs), fpc3 (integer, n_unit * 2L)
#'
#' Additional columns by design:
#'   - design = "replicate": repwt_1 ... repwt_R replicate weight columns
#'   - design = "twophase":  subset (logical, ~40% TRUE), phase1_prob (numeric),
#'     phase2_prob (numeric)
#'
#' @param n         Total number of rows. Default 500L.
#' @param n_psu     Number of PSUs. Default 50L. For BRR/Fay must be even.
#' @param n_ssu     Number of SSUs per PSU. Default NULL (no SSU column).
#'   When non-NULL, adds `ssu` (character, format `"{psu}_s{j}"`) and `fpc2`
#'   (integer, `n_ssu * 2L`) columns.
#' @param n_unit    Number of units per SSU. Default NULL (no unit column).
#'   Requires `n_ssu` to also be non-NULL. When non-NULL, adds `unit`
#'   (character, format `"{ssu}_u{j}"`) and `fpc3` (integer, `n_unit * 2L`).
#' @param n_strata  Number of strata. Default 5L.
#' @param design    Survey design type: "taylor", "replicate", or "twophase".
#' @param type      Replicate weight method (used when design = "replicate").
#'   One of "brr", "jk1", "jk2", "jkn", "bootstrap", "fay". Default "brr".
#' @param phase2_frac  Fraction of Phase 1 units selected into Phase 2
#'   (used when design = "twophase"). Default 0.4.
#' @param with_labels  If TRUE, attach haven-style label/labels attributes to
#'   y1, y2, y3, group, and wt. Default FALSE.
#' @param seed      Random seed for reproducibility. Default 42L.
#'
#' @return A data.frame.
#' @keywords internal
make_survey_data <- function(
  n = 500L,
  n_psu = 50L,
  n_ssu = NULL,
  n_unit = NULL,
  n_strata = 5L,
  design = c("taylor", "replicate", "twophase"),
  type = "brr",
  phase2_frac = 0.4,
  with_labels = FALSE,
  seed = 42L
) {
  design <- match.arg(design)
  type <- tolower(type)

  if (type %in% c("brr", "fay") && n_psu %% 2L != 0L) {
    stop(
      "n_psu must be even for BRR/Fay replicate designs. ",
      "Got n_psu = ",
      n_psu,
      "."
    )
  }

  if (!is.null(n_unit) && is.null(n_ssu)) {
    stop("n_unit requires n_ssu to be specified.")
  }

  set.seed(seed)

  # --- PSU-to-stratum assignment ---
  # Distribute PSUs across strata as evenly as possible
  psus_per_stratum <- rep(n_psu %/% n_strata, n_strata)
  remainder <- n_psu %% n_strata
  if (remainder > 0L) {
    psus_per_stratum[seq_len(remainder)] <- psus_per_stratum[seq_len(
      remainder
    )] +
      1L
  }
  psu_stratum <- rep(seq_len(n_strata), psus_per_stratum) # stratum for each PSU

  # --- Row-to-PSU assignment with varying PSU sizes ---
  # Use Hamilton (largest remainder) method: floor-allocate, then distribute
  # remaining rows to PSUs with largest fractional parts. Guarantees all
  # PSU sizes >= 1 and sum exactly to n.
  psu_sizes_raw <- sample(5:15, n_psu, replace = TRUE)
  exact <- psu_sizes_raw / sum(psu_sizes_raw) * n
  psu_sizes <- as.integer(floor(exact))
  psu_sizes <- pmax(psu_sizes, 1L) # ensure every PSU has >= 1 row
  remaining <- n - sum(psu_sizes)
  if (remaining > 0L) {
    top_idx <- order(exact - floor(exact), decreasing = TRUE)[seq_len(
      remaining
    )]
    psu_sizes[top_idx] <- psu_sizes[top_idx] + 1L
  }

  psu_index <- rep(seq_len(n_psu), times = psu_sizes) # row-level PSU index
  strata <- psu_stratum[psu_index] # row-level stratum index

  # --- FPC: integer population size per stratum ---
  stratum_n <- tabulate(strata, nbins = n_strata)
  stratum_pop <- round(stratum_n * runif(n_strata, 8, 15))
  fpc <- stratum_pop[strata]

  # --- Sampling weights: lognormal variation around stratum base weight ---
  base_wt <- stratum_pop / stratum_n # population/sample ratio per stratum
  wt <- base_wt[strata] * exp(rnorm(n, mean = 0, sd = 0.2))

  # --- Outcome variables ---
  y1 <- rnorm(n, mean = 50, sd = 10)
  y2 <- rnorm(n, mean = 0, sd = 1)
  y3 <- as.integer(runif(n) < 0.3)
  group <- sample(c("A", "B", "C"), n, replace = TRUE)

  df <- data.frame(
    psu = paste0("psu_", psu_index),
    strata = paste0("stratum_", strata),
    fpc = fpc,
    wt = wt,
    y1 = y1,
    y2 = y2,
    y3 = y3,
    group = group,
    stringsAsFactors = FALSE
  )

  # --- SSU column (multi-stage) ---
  if (!is.null(n_ssu)) {
    # Assign SSU IDs round-robin within each PSU
    ssu_index <- integer(n)
    for (p in seq_len(n_psu)) {
      rows_in_psu <- which(psu_index == p)
      ssu_index[rows_in_psu] <- rep_len(
        seq_len(n_ssu),
        length(rows_in_psu)
      )
    }
    df$ssu <- paste0(df$psu, "_s", ssu_index)
    df$fpc2 <- as.integer(n_ssu) * 2L
  }

  # --- Unit column (three-stage) ---
  if (!is.null(n_unit)) {
    # Assign unit IDs round-robin within each SSU
    unit_index <- integer(n)
    unique_ssus <- unique(df$ssu)
    for (s in unique_ssus) {
      rows_in_ssu <- which(df$ssu == s)
      unit_index[rows_in_ssu] <- rep_len(
        seq_len(n_unit),
        length(rows_in_ssu)
      )
    }
    df$unit <- paste0(df$ssu, "_u", unit_index)
    df$fpc3 <- as.integer(n_unit) * 2L
  }

  # --- Replicate weights ---
  if (design == "replicate") {
    R <- switch(
      type,
      brr = n_psu %/% 2L,
      fay = n_psu %/% 2L,
      jk1 = ,
      jk2 = ,
      jkn = ,
      bootstrap = n_psu,
      n_psu
    )
    repwt_matrix <- matrix(
      wt * exp(matrix(rnorm(n * R, mean = 0, sd = 0.1), nrow = n, ncol = R)),
      nrow = n,
      ncol = R
    )
    repwt_df <- as.data.frame(repwt_matrix)
    names(repwt_df) <- paste0("repwt_", seq_len(R))
    df <- cbind(df, repwt_df)
  }

  # --- Two-phase indicator and inclusion probabilities ---
  if (design == "twophase") {
    df$subset <- runif(n) < phase2_frac
    df$phase1_prob <- stratum_n[strata] / stratum_pop[strata]
    df$phase2_prob <- rep(phase2_frac, n)
  }

  # --- Haven-style label attributes ---
  if (with_labels) {
    attr(df$y1, "label") <- "Outcome variable 1 (continuous)"
    attr(df$y2, "label") <- "Outcome variable 2 (continuous)"
    attr(df$y3, "label") <- "Outcome variable 3 (binary, 0/1)"
    attr(df$y3, "labels") <- c("No" = 0L, "Yes" = 1L)
    attr(df$group, "label") <- "Demographic group"
    attr(df$group, "labels") <- c(
      "Group A" = "A",
      "Group B" = "B",
      "Group C" = "C"
    )
    attr(df$wt, "label") <- "Sampling weight"
  }

  df
}

# ------------------------------------------------------------------------------
# test_invariants()
# ------------------------------------------------------------------------------

#' Assert all formal invariants on a survey object
#'
#' Call this at the start of EVERY test_that() block that creates or modifies
#' a survey object. Checks all invariants from formal spec Section I.
#'
#' @param design A survey_taylor, survey_replicate, survey_twophase, or
#'   survey_nonprob object.
#' @return Returns design invisibly on success. Throws testthat failure on any
#'   violated invariant.
#' @keywords internal
test_invariants <- function(design) {
  # All design types must have visible_vars in @variables (may be NULL).
  # Required for surveytidy's select() compatibility.
  if (!S7::S7_inherits(design, survey_nonprob)) {
    testthat::expect_true(
      "visible_vars" %in% names(design@variables),
      label = "@variables has 'visible_vars' key"
    )
  }

  # survey_nonprob has a different @variables structure — handle separately
  if (S7::S7_inherits(design, survey_nonprob)) {
    testthat::expect_true(is.data.frame(design@data))
    testthat::expect_false(is.null(design@data))
    testthat::expect_gte(nrow(design@data), 1L)
    testthat::expect_false(
      anyDuplicated(names(design@data)) > 0L,
      label = "@data has no duplicate column names"
    )

    testthat::expect_true(
      "weights" %in% names(design@variables),
      label = "@variables has 'weights' key"
    )
    testthat::expect_true(
      "probs_provided" %in% names(design@variables),
      label = "@variables has 'probs_provided' key"
    )

    wt_var <- design@variables$weights
    if (!is.null(wt_var)) {
      testthat::expect_true(
        wt_var %in% names(design@data),
        label = paste0("weight column '", wt_var, "' present in @data")
      )
      wt_col <- design@data[[wt_var]]
      testthat::expect_true(
        is.numeric(wt_col),
        label = "weight column is numeric"
      )
      testthat::expect_true(
        sum(!is.na(wt_col)) > 0L,
        label = "weight column has at least one non-NA value"
      )
      testthat::expect_true(
        all(wt_col[!is.na(wt_col)] >= 0),
        label = "weight column has all non-negative non-NA values"
      )
      testthat::expect_true(
        any(wt_col[!is.na(wt_col)] > 0),
        label = "weight column has at least one positive non-NA value"
      )
    }

    testthat::expect_true(S7::S7_inherits(design@metadata, survey_metadata))
    meta_vars <- names(design@metadata@variable_labels)
    if (length(meta_vars) > 0L) {
      testthat::expect_true(
        all(meta_vars %in% names(design@data)),
        label = "all metadata-labelled vars present in @data"
      )
    }

    # All 5 repweight keys must be present (values may be NULL).
    for (key in c("repweights", "type", "scale", "rscales", "mse")) {
      testthat::expect_true(
        key %in% names(design@variables),
        label = paste0("@variables has '", key, "' key")
      )
    }

    return(invisible(design))
  }
  # Invariant 1: @data is a non-NULL data.frame with >= 1 row, no duplicate names
  testthat::expect_true(is.data.frame(design@data))
  testthat::expect_false(is.null(design@data))
  testthat::expect_gte(nrow(design@data), 1L)
  testthat::expect_false(
    anyDuplicated(names(design@data)) > 0L,
    label = "@data has no duplicate column names"
  )

  # Invariant 2: All named design columns exist in @data and are atomic.
  # survey_twophase has a nested variables structure; handle it separately.
  if (S7::S7_inherits(design, survey_twophase)) {
    p1 <- design@variables$phase1
    p2 <- design@variables$phase2
    design_vars <- c(
      p1$ids,
      p1$weights,
      p1$strata,
      p1$fpc,
      if (!is.null(p2)) c(p2$ids, p2$strata, p2$probs, p2$fpc),
      design@variables$subset
    )
  } else {
    design_vars <- c(
      design@variables$ids,
      design@variables$weights,
      design@variables$strata,
      design@variables$fpc
    )
  }
  design_vars <- design_vars[!is.null(design_vars)]
  for (v in design_vars) {
    testthat::expect_true(
      v %in% names(design@data),
      label = paste0("design var '", v, "' present in @data")
    )
    testthat::expect_true(
      is.atomic(design@data[[v]]),
      label = paste0("design var '", v, "' is atomic")
    )
  }

  # Invariant 3: Weights are numeric and strictly positive.
  # For survey_twophase, weights live under @variables$phase1$weights.
  wt_var <- if (S7::S7_inherits(design, survey_twophase)) {
    design@variables$phase1$weights
  } else {
    design@variables$weights
  }
  if (!is.null(wt_var)) {
    wt_col <- design@data[[wt_var]]
    testthat::expect_true(
      is.numeric(wt_col),
      label = paste0("weight column '", wt_var, "' is numeric")
    )
    testthat::expect_true(
      all(wt_col[!is.na(wt_col)] > 0),
      label = paste0("weight column '", wt_var, "' has all positive values")
    )
  }

  # Invariant 4: Replicate weights are numeric (survey_replicate only)
  if (S7::S7_inherits(design, survey_replicate)) {
    for (rw in design@variables$repweights) {
      testthat::expect_true(
        is.numeric(design@data[[rw]]),
        label = paste0("replicate weight column '", rw, "' is numeric")
      )
    }
  }

  # Additional check for survey_twophase: subset column must be logical
  if (S7::S7_inherits(design, survey_twophase)) {
    subset_var <- design@variables$subset
    if (!is.null(subset_var) && subset_var %in% names(design@data)) {
      testthat::expect_true(
        is.logical(design@data[[subset_var]]),
        label = paste0("subset column '", subset_var, "' is logical")
      )
    }
  }

  # Invariant 5: @metadata is a survey_metadata object; all labelled vars in @data
  testthat::expect_true(S7::S7_inherits(design@metadata, survey_metadata))
  meta_vars <- names(design@metadata@variable_labels)
  if (length(meta_vars) > 0L) {
    testthat::expect_true(
      all(meta_vars %in% names(design@data)),
      label = "all metadata-labelled vars present in @data"
    )
  }

  invisible(design)
}

# ------------------------------------------------------------------------------
# test_collection_invariants()
# ------------------------------------------------------------------------------

#' Assert all formal invariants on a survey_collection object
#'
#' Call this as the FIRST assertion in every test_that() block that creates
#' a survey_collection. Enforces the five invariants from
#' plans/spec-survey-collection.md §3.2.
#'
#' @param coll A survey_collection object.
#' @return Returns coll invisibly on success. Throws testthat failure on any
#'   violated invariant.
#' @keywords internal
test_collection_invariants <- function(coll) {
  # Invariant 1: inherits survey_collection
  testthat::expect_true(
    S7::S7_inherits(coll, survey_collection),
    label = "inherits from survey_collection"
  )

  # Invariant 2: @surveys is a list of length >= 1
  testthat::expect_true(is.list(coll@surveys))
  testthat::expect_gte(length(coll@surveys), 1L)

  # Invariant 3: fully named, no empty, no NA, no duplicates
  nms <- names(coll@surveys)
  testthat::expect_false(is.null(nms), label = "@surveys has names")
  testthat::expect_false(
    any(nms == ""),
    label = "@surveys has no empty names"
  )
  testthat::expect_false(
    any(is.na(nms)),
    label = "@surveys has no NA names"
  )
  testthat::expect_equal(
    anyDuplicated(nms),
    0L,
    label = "@surveys has no duplicate names"
  )

  # Invariant 4: every element inherits survey_base
  elem_ok <- vapply(
    coll@surveys,
    function(x) S7::S7_inherits(x, survey_base),
    logical(1L)
  )
  testthat::expect_true(
    all(elem_ok),
    label = "every element inherits survey_base"
  )

  # Invariant 5: the collection itself does NOT inherit survey_base
  testthat::expect_false(
    S7::S7_inherits(coll, survey_base),
    label = "survey_collection does NOT inherit survey_base"
  )

  invisible(coll)
}

# ------------------------------------------------------------------------------
# test_collection_groups_invariant()
# ------------------------------------------------------------------------------

#' Assert the uniform-grouping invariant across a survey_collection
#'
#' Every member's `@groups` must be `identical()` to `coll@groups`. Call this
#' as the second assertion (after `test_invariants(coll)`) in every
#' `test_that()` block that constructs a `survey_collection`.
#'
#' @param coll A `survey_collection` object.
#' @return Invisibly `coll`. Throws testthat failure on any violation.
#' @keywords internal
test_collection_groups_invariant <- function(coll) {
  for (nm in names(coll@surveys)) {
    testthat::expect_identical(
      coll@surveys[[nm]]@groups,
      coll@groups,
      label = paste0("member ", nm, "@groups matches coll@groups")
    )
  }
  invisible(coll)
}

# ------------------------------------------------------------------------------
# test_result_invariants()
# ------------------------------------------------------------------------------

#' Assert all formal invariants on a survey result object
#'
#' Call this as the FIRST assertion in every test_that() block that creates a
#' result from a get_*() function. The direct parallel to test_invariants()
#' for design objects.
#'
#' @param result A survey_freqs, survey_means, survey_totals, survey_corr,
#'   survey_quantiles, or survey_ratios object.
#' @param expected_class The specific S3 class string expected (e.g.,
#'   "survey_means").
#' @return Returns result invisibly on success. Throws testthat failure on any
#'   violated invariant.
#' @keywords internal
test_result_invariants <- function(result, expected_class) {
  # 1. Correct S3 class hierarchy
  testthat::expect_true(
    inherits(result, expected_class),
    label = paste0("result inherits from ", expected_class)
  )
  testthat::expect_true(
    inherits(result, "survey_result"),
    label = "result inherits from survey_result"
  )
  testthat::expect_true(
    tibble::is_tibble(result),
    label = "result is a tibble"
  )

  # 2. meta() returns a non-NULL list
  m <- meta(result)
  testthat::expect_false(is.null(m), label = "meta() is not NULL")
  testthat::expect_type(m, "list")

  # 3. Required meta keys always present (never absent)
  required_common_keys <- c("design_type", "conf_level", "call", "group")
  testthat::expect_true(
    all(required_common_keys %in% names(m)),
    label = "all required meta keys present"
  )

  # 4. group is always a list (empty list when no groups active)
  testthat::expect_type(m$group, "list")

  # 5. n_respondents: always present and a positive integer
  testthat::expect_true(
    "n_respondents" %in% names(m),
    label = "n_respondents key present in meta"
  )
  testthat::expect_type(m$n_respondents, "integer")
  testthat::expect_gt(m$n_respondents, 0L, label = "n_respondents > 0")

  invisible(result)
}

# ------------------------------------------------------------------------------
# make_all_designs()
# ------------------------------------------------------------------------------

#' Create all three survey design types for cross-design testing
#'
#' Convenience wrapper that returns one survey object of each class.
#' Use in verb tests to iterate over all design types with a for loop.
#'
#' @param seed   Random seed passed to make_survey_data(). Default 42L.
#' @return A named list with elements: srs, taylor, replicate, twophase, calibrated.
#' @keywords internal
make_all_designs <- function(seed = 42L) {
  # Taylor series design
  df_t <- make_survey_data(
    n = 100L,
    n_psu = 10L,
    n_strata = 2L,
    design = "taylor",
    seed = seed
  )
  taylor <- as_survey(
    df_t,
    ids = psu,
    weights = wt,
    strata = strata,
    fpc = fpc,
    nest = TRUE
  )

  # Replicate weights design
  df_r <- make_survey_data(
    n = 100L,
    n_psu = 10L,
    n_strata = 2L,
    design = "replicate",
    type = "brr",
    seed = seed
  )
  repwt_cols <- grep("^repwt_", names(df_r), value = TRUE)
  replicate <- as_survey_replicate(
    df_r,
    weights = wt,
    repweights = tidyselect::all_of(repwt_cols),
    type = "BRR"
  )

  # Two-phase design
  df_p <- make_survey_data(
    n = 100L,
    n_psu = 10L,
    n_strata = 2L,
    design = "twophase",
    seed = seed
  )
  phase1 <- as_survey(
    df_p,
    ids = psu,
    weights = wt,
    strata = strata,
    fpc = fpc,
    nest = TRUE
  )
  twophase <- as_survey_twophase(phase1, subset = subset, method = "approx")

  # Calibrated design (non-probability, weights only)
  df_c <- make_survey_data(
    n = 100L,
    n_psu = 10L,
    n_strata = 2L,
    design = "taylor",
    seed = seed
  )
  calibrated <- as_survey_nonprob(df_c, weights = wt)

  # Simple random sample (Taylor with no ids/strata)
  df_s <- make_survey_data(
    n = 100L,
    n_psu = 10L,
    n_strata = 2L,
    design = "taylor",
    seed = seed
  )
  srs <- as_survey(df_s, weights = wt)

  list(
    srs = srs,
    taylor = taylor,
    replicate = replicate,
    twophase = twophase,
    calibrated = calibrated
  )
}

# ------------------------------------------------------------------------------
# make_na_group_design()
# ------------------------------------------------------------------------------

#' Create a survey design with NA values in the group variable
#'
#' Returns a survey_taylor object with a `grp` column (~20% NA) and a `grp2`
#' column (no NAs). Used for NA group row tests in all six analysis functions.
#'
#' set.seed(seed + 1L) isolates the grp/grp2 RNG from make_survey_data()'s
#' internal RNG consumption, so fixture values are stable even if
#' make_survey_data() changes internally.
#'
#' @param n        Total rows. Default 200.
#' @param na_frac  Fraction of grp values to set NA. Default 0.2.
#' @param seed     Random seed. Default 42.
#' @return A survey_taylor object.
#' @keywords internal
make_na_group_design <- function(n = 200, na_frac = 0.2, seed = 42) {
  df <- make_survey_data(n = n, seed = seed)
  set.seed(seed + 1L)
  na_idx <- sample(seq_len(n), size = floor(n * na_frac))
  df$grp <- sample(c("A", "B", "C"), n, replace = TRUE)
  df$grp[na_idx] <- NA_character_
  df$grp2 <- sample(c("X", "Y"), n, replace = TRUE)
  as_survey(
    df,
    ids = psu,
    weights = wt,
    strata = strata,
    fpc = fpc,
    nest = TRUE
  )
}

# ------------------------------------------------------------------------------
# make_all_na_group_design()
# ------------------------------------------------------------------------------

#' Create a survey design with an all-NA group variable
#'
#' Returns a survey_taylor object where `grp` is entirely NA. Used for
#' Test Block 6 (all-NA group var edge case) in NA group row tests.
#'
#' @param n    Total rows. Default 100.
#' @param seed Random seed. Default 1.
#' @return A survey_taylor object.
#' @keywords internal
make_all_na_group_design <- function(n = 100, seed = 1) {
  df <- make_survey_data(n = n, seed = seed)
  df$grp <- NA_character_
  as_survey(
    df,
    ids = psu,
    weights = wt,
    strata = strata,
    fpc = fpc,
    nest = TRUE
  )
}

# ------------------------------------------------------------------------------
# get_na_group_rows()
# ------------------------------------------------------------------------------

#' Extract rows where the named group column is NA
#'
#' DRY helper used in Test Blocks 3, 4, 6, 9, 10, and all oracle tests.
#' Prevents repeating `result[is.na(result[[group_col]]), ]` across 48+
#' test blocks.
#'
#' @param result    A survey result tibble from any get_*() function.
#' @param group_col Character(1): the group column name to check for NA.
#' @return A tibble with only the rows where group_col is NA.
#' @keywords internal
get_na_group_rows <- function(result, group_col) {
  result[is.na(result[[group_col]]), ]
}

# ------------------------------------------------------------------------------
# test_glm_fit_invariants()
# ------------------------------------------------------------------------------

#' Assert all formal invariants on a survey_glm_fit object
#'
#' Call this as the FIRST assertion in every test_that() block that calls
#' survey_glm() and expects a valid fit. Checks all invariants from spec
#' Section 9.3a.
#'
#' @param fit A survey_glm_fit object.
#' @return Returns fit invisibly on success. Throws testthat failure on any
#'   violated invariant.
#' @keywords internal
test_glm_fit_invariants <- function(fit) {
  p <- length(fit@coefficients)
  # 1. Correct S7 class
  testthat::expect_true(S7::S7_inherits(fit, survey_glm_fit))
  # 2. Coefficients non-empty
  testthat::expect_true(p > 0)
  # 3. vcov is p x p
  testthat::expect_identical(dim(fit@vcov), c(p, p))
  # 4. degf > 0
  testthat::expect_gt(fit@degf, 0)
  # 5. converged is logical
  testthat::expect_type(fit@converged, "logical")
  # 6. formula is a formula object
  testthat::expect_true(inherits(fit@formula, "formula"))
  invisible(fit)
}

# ------------------------------------------------------------------------------
# test_glm_tidy_invariants()
# ------------------------------------------------------------------------------

#' Assert all formal invariants on a survey_glm_tidy (clean()) result
#'
#' Call this as the FIRST assertion in every test_that() block that calls
#' clean() and expects a valid tidy result. Checks all invariants from spec
#' Section 9.3.
#'
#' @param result A survey_glm_tidy object (output of clean()).
#' @return Returns result invisibly on success. Throws testthat failure on any
#'   violated invariant.
#' @keywords internal
test_glm_tidy_invariants <- function(result) {
  # 1. Correct S3 class hierarchy
  testthat::expect_true(inherits(result, "survey_glm_tidy"))
  testthat::expect_true(inherits(result, "survey_result"))
  testthat::expect_true(tibble::is_tibble(result))
  # 2. Required columns always present
  expected_cols <- c(
    "term",
    "variable",
    "var_label",
    "label",
    "reference_row",
    "estimate",
    "std_error",
    "p_value",
    "conf_low",
    "conf_high"
  )
  testthat::expect_true(all(expected_cols %in% names(result)))
  # 3. reference_row is logical, no NAs
  testthat::expect_type(result$reference_row, "logical")
  testthat::expect_false(anyNA(result$reference_row))
  # 4. label is character, never NA
  testthat::expect_type(result$label, "character")
  testthat::expect_false(anyNA(result$label))
  # 5. meta() returns non-NULL list with all 15 required keys
  m <- meta(result)
  testthat::expect_false(is.null(m))
  required_keys <- c(
    "formula",
    "family",
    "link",
    "design_type",
    "conf_level",
    "call",
    "group_names",
    "group_labels",
    "n_observations",
    "n_weighted",
    "degf",
    "exponentiate",
    "include_reference",
    "converged",
    "variables"
  )
  testthat::expect_true(all(required_keys %in% names(m)))
  # 6. group_names is always character(0) for regression
  testthat::expect_identical(m$group_names, character(0))
  # 7. n_observations is positive integer
  testthat::expect_type(m$n_observations, "integer")
  testthat::expect_gt(m$n_observations, 0L)
  # 8. n_weighted is positive numeric
  testthat::expect_type(m$n_weighted, "double")
  testthat::expect_gt(m$n_weighted, 0)
  # 9. degf is positive numeric
  testthat::expect_type(m$degf, "double")
  testthat::expect_gt(m$degf, 0)
  # 10. $variables is a named list; each entry has all 7 required sub-keys
  vars <- m$variables
  testthat::expect_type(vars, "list")
  var_subkeys <- c(
    "var_label",
    "var_class",
    "var_type",
    "var_nlevels",
    "contrasts",
    "reference_level",
    "value_labels"
  )
  for (v in vars) {
    testthat::expect_true(all(var_subkeys %in% names(v)))
    testthat::expect_type(v$var_label, "character") # never NULL; falls back to name
    testthat::expect_false(is.null(v$var_label))
  }
  invisible(result)
}

# ------------------------------------------------------------------------------
# .testhelper_clobber_domain()
# ------------------------------------------------------------------------------

#' Mutate the `..surveycore_domain..` column on a fit's design
#'
#' Test-only helper. Used to construct the A-20 trigger in get_anova() tests:
#' fit a full model, then rewrite the stored design's domain indicator so the
#' reduced-fit defensive assertion in `.refit_drop_terms()` fires.
#'
#' The S7 property slots are read-only by default; this helper chains
#' `S7::set_props()` on both the design and the fit to replace the @data slot
#' on the design, then re-attach the modified design to the fit.
#'
#' @param fit A `survey_glm_fit` whose design has a `..surveycore_domain..`
#'   column (e.g. from `update_design()` / `filter()`).
#' @param value Logical vector of length `nrow(fit@design@data)` — the new
#'   domain indicator values.
#' @return The `survey_glm_fit` with `@design@data$..surveycore_domain..`
#'   set to `value`.
#' @keywords internal
.testhelper_clobber_domain <- function(fit, value) {
  df <- fit@design@data
  df[["..surveycore_domain.."]] <- value
  new_design <- S7::set_props(fit@design, data = df)
  S7::set_props(fit, design = new_design)
}

# ------------------------------------------------------------------------------
# make_replicate_nonconverger()
# ------------------------------------------------------------------------------

#' Build a replicate design / fit pair that triggers A-19 on refit
#'
#' 50-row dataset with a binary factor whose rare level appears in exactly one
#' PSU. JK1-style replicate weights with `n_strata = n_psu` (each PSU its own
#' stratum) mean the rare-level PSU is fully dropped in one replicate. A
#' binomial(logit) GLM where the rare level is the only positive case
#' guarantees quasi-separation on the dropped replicate — deterministic across
#' BLAS libraries.
#'
#' Returned list:
#'   - `design` : a `survey_replicate` object
#'   - `formula`: a formula whose reduced-fit refit will exhibit
#'                replicate non-convergence
#'
#' @param seed Random seed. Default 42.
#' @return A named list with elements `design` and `formula`.
#' @keywords internal
make_replicate_nonconverger <- function(seed = 42L) {
  set.seed(seed)
  n <- 50L
  n_psu <- 10L
  psu_id <- rep(seq_len(n_psu), each = n %/% n_psu)
  # rare: level "B" only on PSU 1; all other PSUs get level "A"
  rare <- ifelse(psu_id == 1L, "B", "A")
  rare <- factor(rare, levels = c("A", "B"))
  # y: binary, all positives concentrated in PSU 1 (quasi-separation)
  y <- as.integer(psu_id == 1L)
  cov <- rnorm(n)
  wt <- rep(1, n)
  # JK1 replicate weights: one replicate per PSU, drop that PSU.
  repwts <- matrix(wt, nrow = n, ncol = n_psu)
  for (r in seq_len(n_psu)) {
    mask <- psu_id == r
    if (any(mask)) {
      # Scale up non-dropped PSUs so sum(weights) matches across replicates.
      scale <- sum(wt) / sum(wt[!mask])
      repwts[mask, r] <- 0
      repwts[!mask, r] <- wt[!mask] * scale
    }
  }
  repwt_df <- as.data.frame(repwts)
  names(repwt_df) <- paste0("repwt_", seq_len(n_psu))
  df <- cbind(
    data.frame(
      psu = paste0("psu_", psu_id),
      y = y,
      rare = rare,
      cov = cov,
      wt = wt,
      stringsAsFactors = FALSE
    ),
    repwt_df
  )
  design <- as_survey_replicate(
    df,
    weights = wt,
    repweights = tidyselect::all_of(names(repwt_df)),
    type = "JK1"
  )
  list(
    design = design,
    formula = y ~ rare + cov
  )
}

# ------------------------------------------------------------------------------
# .hand_polyserial_twostep() — strict two-step MLE oracle for polyserial tests
# ------------------------------------------------------------------------------

#' Hand-computed two-step MLE oracle for polyserial correlation
#'
#' `polycor::polyserial()` uses either a joint MLE (`ML = TRUE`) or Drasgow's
#' approximation (`ML = FALSE`); neither matches the two-step Cox (1974) /
#' Mannan 2025 §5.1 construction that surveycore's `.corr_polyserial_mle()`
#' implements. This helper is the hand-computed two-step MLE — the exact
#' mathematical oracle our implementation targets at 1e-6.
#'
#' @param ord integer vector of ordinal codes (1..K)
#' @param cont numeric continuous vector
#' @return ρ̂ from the two-step MLE
#' @keywords internal
#' @noRd
.hand_polyserial_twostep <- function(ord, cont) {
  n <- length(ord)
  k <- length(unique(ord))
  marginal <- cumsum(tabulate(ord, nbins = k)) / n
  thresholds <- stats::qnorm(marginal[-length(marginal)])
  mu <- mean(cont)
  sig <- sqrt(mean((cont - mu)^2))
  z <- (cont - mu) / sig
  loglik <- function(rho) {
    denom <- sqrt(1 - rho^2)
    t_full <- c(-Inf, thresholds, Inf)
    p <- mapply(function(zi, mi) {
      u_hi <- (t_full[mi + 1L] - rho * zi) / denom
      u_lo <- (t_full[mi] - rho * zi) / denom
      stats::pnorm(u_hi) - stats::pnorm(u_lo)
    }, z, ord)
    sum(log(p))
  }
  fit <- stats::optimize(loglik, c(-1 + 1e-6, 1 - 1e-6), maximum = TRUE)
  fit$maximum
}
