# R/utils.R
#
# Utility functions used across two or more source files.
# Single-use helpers live at the top of their respective source files.
# See .claude/rules/code-style.md Section 4 for placement rules.
#
# Note: .update_design_var_names() and .rename_metadata_keys() live in
# R/02-validators.R because they were first needed by the validator test
# infrastructure and are co-located with their associated validation logic.

# ── .glm_confint() ────────────────────────────────────────────────────────────
#
# Shared CI helper for survey_glm_fit. Called by both confint.survey_glm_fit()
# (in glm-methods.R) and clean() (in glm-clean.R). Single implementation
# guarantees numerical identity between the two callers.
#
# Formula: estimate ± qt((1 + level) / 2, df = degf_design - (n_coef - 1)) * se
#
# @param estimates   Named numeric vector of coefficient estimates.
# @param se          Numeric vector of standard errors (same length).
# @param degf_design Design degrees of freedom (model@degf). Finite value.
# @param n_coef      Total number of coefficients p = length(estimates).
# @param level       Confidence level in (0, 1). Validated upstream.
# @param parm        Character or integer index to subset; NULL = all.
# @return Two-column numeric matrix with columns "lower" and "upper".
#' @noRd
.glm_confint <- function(
  estimates,
  se,
  degf_design,
  n_coef,
  level,
  parm = NULL
) {
  if (!is.null(parm)) {
    estimates <- estimates[parm]
    se <- se[parm]
  }
  df_res <- max(1, degf_design - (n_coef - 1L))
  half_w <- stats::qt((1 + level) / 2, df = df_res) * se
  matrix(
    c(estimates - half_w, estimates + half_w),
    ncol = 2L,
    dimnames = list(names(estimates), c("lower", "upper"))
  )
}


# ── Exported accessor ─────────────────────────────────────────────────────────

#' Access the Data Component of a Survey Design Object
#'
#' Returns the underlying data frame stored in a survey design object.
#' This is a thin accessor for `x@data` that provides a stable public name
#' independent of the S7 property structure.
#'
#' @param x A `survey_taylor`, `survey_replicate`, or `survey_twophase` object.
#' @return A `data.frame` with all variables, including design variables.
#' @examples
#' d <- as_survey(
#'   nhanes_2017,
#'   ids = sdmvpsu,
#'   weights = wtint2yr,
#'   strata = sdmvstra,
#'   nest = TRUE
#' )
#' head(survey_data(d))
#' @family accessors
#' @export
survey_data <- function(x) {
  if (!S7::S7_inherits(x, survey_base)) {
    cli::cli_abort(
      c(
        "x" = "{.arg x} must be a survey design object.",
        "i" = "Got {.cls {class(x)[[1L]]}}."
      ),
      class = "surveycore_error_not_survey_object"
    )
  }
  x@data
}


#' Extract the Weighting History from a Survey Object
#'
#' Returns the list of weighting operations recorded on a survey design object.
#' Each entry is appended by surveywts after a calibration or nonresponse
#' adjustment step. Returns an empty list when no history has been recorded.
#'
#' @param x A survey design object (any class inheriting from `survey_base`).
#' @return A `list` of history entries, or `list()` if no history is present.
#'
#' @examples
#' d <- as_survey(
#'   nhanes_2017,
#'   ids = sdmvpsu,
#'   weights = wtint2yr,
#'   strata = sdmvstra,
#'   nest = TRUE
#' )
#' survey_weighting_history(d) # list() — no weighting history
#' @family metadata
#' @export
survey_weighting_history <- function(x) {
  if (!S7::S7_inherits(x, survey_base)) {
    cli::cli_abort(
      c(
        "x" = "{.arg x} must be a survey design object.",
        "i" = "Got {.cls {class(x)[[1L]]}}."
      ),
      class = "surveycore_error_not_survey_object"
    )
  }
  x@metadata@weighting_history
}


# ── Exported constants ────────────────────────────────────────────────────────

#' Internal Domain Column Name Constant
#'
#' The name of the logical column added to `@data` by `filter()` (from
#' `surveytidy`) to mark domain membership. Exposed here so that sibling
#' packages (`surveytidy`, `surveywts`) can reference it without
#' using `:::`.
#'
#' @export
SURVEYCORE_DOMAIN_COL <- "..surveycore_domain.."


# ── Internal constants ────────────────────────────────────────────────────────

# Name of the synthetic weight column added to @data when the user supplies
# neither weights nor probs (SRS fallback) or when probs are converted to
# weights internally. Using a named constant prevents typos and makes a
# sentinel rename safe.
.SURVEYCORE_WT_COL <- "..surveycore_wt.."


# ── Internal tidy-select resolvers ───────────────────────────────────────────

# Resolve a tidy-select quosure against a data frame.
# Returns a character vector of selected column names, or NULL when expr is a
# NULL quosure. Count validation is left to callers — error classes differ by
# argument (weights, strata, fpc, etc.).
# Used by constructors and by surveytidy dplyr verbs.
#
# @param expr  A quosure (from rlang::enquo()). NULL quosure → returns NULL.
# @param data  A data.frame to evaluate the selection against.
# @return Character vector of column names, or NULL.
#' @noRd
.resolve_tidy_select <- function(expr, data) {
  if (rlang::quo_is_null(expr)) {
    return(NULL)
  }
  tryCatch(
    names(tidyselect::eval_select(expr, data)),
    vctrs_error_subscript_oob = function(cnd) {
      missing <- cnd$i
      if (is.null(missing)) {
        missing <- conditionMessage(cnd)
      }
      have <- names(data)
      cli::cli_abort(
        c(
          "x" = "Variable{?s} {.val {missing}} not found in survey data.",
          "i" = "Available: {.val {have}}."
        ),
        class = "surveycore_error_variable_not_found"
      )
    }
  )
}

# Resolve a tidy-select quosure that must select EXACTLY ONE column.
# Returns the single column name on success, or NULL if expr is a NULL
# quosure (i.e., the argument was omitted / defaulted to NULL).
#
# Fires typed errors when 0 or > 1 columns are selected, so callers can
# pass argument-specific error classes rather than inlining the 12-line
# boilerplate for every single-column argument.
#
# @param expr        A quosure (from rlang::enquo()).
# @param data        A data.frame to evaluate the selection against.
# @param arg_nm      Character(1). Argument name shown in error messages.
# @param required    Logical(1). If TRUE, a NULL quosure → abort (missing).
#                    If FALSE (default), a NULL quosure → return NULL.
# @param class_none  Error class when 0 columns selected.
# @param class_multi Error class when > 1 column selected.
# @return Character(1) column name, or NULL.
#' @noRd
.resolve_single_col <- function(
  expr,
  data,
  arg_nm,
  required = FALSE,
  class_none = "surveycore_error_design_var_missing",
  class_multi = "surveycore_error_design_var_missing",
  call = rlang::caller_call()
) {
  if (rlang::quo_is_null(expr)) {
    if (required) {
      cli::cli_abort(
        c("x" = "{.arg {arg_nm}} is required but was not provided."),
        class = class_none,
        call = call
      )
    }
    return(NULL)
  }

  cols <- tidyselect::eval_select(expr, data)

  if (length(cols) == 0L) {
    cli::cli_abort(
      c("x" = "{.arg {arg_nm}} matched no columns in {.arg data}"),
      class = class_none,
      call = call
    )
  }

  if (length(cols) > 1L) {
    cli::cli_abort(
      c(
        "x" = paste0(
          "{.arg {arg_nm}} must select exactly one column, ",
          "not {length(cols)}"
        )
      ),
      class = class_multi,
      call = call
    )
  }

  names(cols)
}


# ── Internal design-variable helpers ─────────────────────────────────────────

#' Get design variable column names
#'
#' Returns a flat character vector of all design-variable column names
#' (ids, weights, strata, fpc) for any survey design class. `NULL` entries
#' are dropped; names are unique. Exported for use by extension packages
#' (e.g., `surveytidy`); not intended for end users.
#'
#' @param design A survey design object (`survey_base` subclass).
#' @return A character vector of column names.
#' @export
.get_design_vars_flat <- function(design) {
  if (S7::S7_inherits(design, survey_taylor)) {
    unique(c(
      design@variables$ids,
      design@variables$weights,
      design@variables$strata,
      design@variables$fpc
    ))
  } else if (S7::S7_inherits(design, survey_replicate)) {
    unique(c(
      design@variables$weights,
      design@variables$repweights
    ))
  } else if (S7::S7_inherits(design, survey_twophase)) {
    p1 <- design@variables$phase1
    p2 <- design@variables$phase2
    p2_cols <- if (!is.null(p2)) {
      unlist(p2[!vapply(p2, is.null, logical(1L))], use.names = FALSE)
    } else {
      # nocov — p2 is always a list (both constructors initialize it)
      character(0L)
    }
    unique(c(
      p1$ids,
      p1$weights,
      p1$strata,
      p1$fpc,
      p2_cols,
      design@variables$subset
    ))
  } else if (S7::S7_inherits(design, survey_nonprob)) {
    unique(c(design@variables$weights))
  } else {
    character(0L) # nocov — defensive: all known types handled above
  }
}

# Return a named list mapping slot names to column name(s).
# Slots whose value is NULL are omitted from the result.
# unlist()ing the result gives all design variable names.
# Used by surveytidy dplyr verbs (rename, select) for slot-level granularity
# and by conversion methods to identify which role each column plays.
#' @noRd
.get_design_vars <- function(design) {
  if (S7::S7_inherits(design, survey_taylor)) {
    Filter(
      Negate(is.null),
      list(
        ids = design@variables$ids,
        weights = design@variables$weights,
        strata = design@variables$strata,
        fpc = design@variables$fpc
      )
    )
  } else if (S7::S7_inherits(design, survey_replicate)) {
    Filter(
      Negate(is.null),
      list(
        weights = design@variables$weights,
        repweights = design@variables$repweights
      )
    )
  } else if (S7::S7_inherits(design, survey_twophase)) {
    p1 <- design@variables$phase1
    p2 <- design@variables$phase2
    raw <- list(
      ids = p1$ids,
      weights = p1$weights,
      strata = p1$strata,
      fpc = p1$fpc,
      ids2 = if (!is.null(p2)) p2$ids else NULL,
      strata2 = if (!is.null(p2)) p2$strata else NULL,
      probs2 = if (!is.null(p2)) p2$probs else NULL,
      fpc2 = if (!is.null(p2)) p2$fpc else NULL,
      subset = design@variables$subset
    )
    Filter(Negate(is.null), raw)
  } else if (S7::S7_inherits(design, survey_nonprob)) {
    Filter(
      Negate(is.null),
      list(weights = design@variables$weights)
    )
  } else {
    list() # nocov — defensive: all known types handled above
  }
}


# ── Internal: weighting history promotion ────────────────────────────────────

# Promote a weighting_history attribute from a data frame to a metadata object.
# Called by constructors that accept a raw data frame (as_survey,
# as_survey_replicate). Returns the metadata object unchanged when the
# attribute is absent or is not a non-empty list.
#
# @param data     A data.frame (may or may not have "weighting_history" attr).
# @param metadata A survey_metadata object (already populated by
#                 .extract_haven_metadata()).
# @return The survey_metadata object, with @weighting_history set if present.
#' @noRd
.promote_weighting_history <- function(data, metadata) {
  history <- attr(data, "weighting_history", exact = TRUE)
  if (is.list(history) && length(history) > 0L) {
    metadata@weighting_history <- history
  }
  metadata
}


# ── .build_cluster_matrices() ───────────────────────────────────────────────
#
# Build the clusters, strata, and FPC matrices needed by the multi-stage
# Taylor variance engine (.svy_recvar / .svy_multistage). Generalizes the
# single-stage logic from .taylor_build_inputs() to k >= 1 stages.
#
# Used across 8+ files (variance-taylor.R, variance-srs.R, etc.); lives in
# utils.R per code-style.md Section 4 internal helper placement rule.
#
# @param data A data.frame (the @data from the survey design object).
# @param vars A list (the @variables from the survey design object).
# @return A named list: clusters_mat (integer matrix n x k),
#   strata_mat (integer matrix n x k), fpcs (list with sampsize and
#   popsize matrices).
#' @noRd
.build_cluster_matrices <- function(data, vars) {
  n <- nrow(data)

  # Step 1: Determine number of stages
  k <- max(1L, if (!is.null(vars$ids)) length(vars$ids) else 0L)

  # Step 2: Build stage-1 stratum vector
  strata_id <- if (!is.null(vars$strata)) {
    data[[vars$strata]]
  } else {
    rep(1L, n)
  }

  # Step 3: Build clusters matrix (k columns)

  # Column 1 — stage-1 PSU IDs (with nest adjustment)
  psu_id <- if (!is.null(vars$ids)) {
    raw_ids <- data[[vars$ids[[1L]]]]
    if (isTRUE(vars$nest) && !is.null(vars$strata)) {
      as.integer(interaction(strata_id, raw_ids, drop = TRUE))
    } else {
      as.integer(interaction(raw_ids, drop = TRUE))
    }
  } else {
    seq_len(n) # each row is its own PSU
  }

  # Columns 2..k — sub-unit IDs (globally unique via interaction())
  # IMPORTANT: Never use `2:k` directly. In R, `2:1` evaluates to c(2L, 1L),
  # not an empty vector. The `if (k > 1L)` guard is mandatory.
  extra_cols <- if (k > 1L) {
    # Pre-allocate column 1 into a temp matrix so we can reference it
    # during the loop
    cols <- vector("list", k - 1L)
    prev_col <- psu_id
    for (j in seq(2L, k)) {
      cur_col <- as.integer(
        interaction(prev_col, data[[vars$ids[[j]]]], drop = TRUE)
      )
      cols[[j - 1L]] <- cur_col
      prev_col <- cur_col
    }
    cols
  } else {
    list()
  }

  clusters_mat <- matrix(
    data = c(psu_id, unlist(extra_cols)),
    nrow = n,
    ncol = k
  )

  # Step 4: Build strata matrix (k columns)
  # Column 1: strata_id (integer-coded)
  # Column j (j = 2..k): parent cluster ID = clusters_mat[, j-1]
  strata_col1 <- as.integer(interaction(strata_id, drop = TRUE))
  extra_strata <- if (k > 1L) {
    lapply(seq(2L, k), function(j) clusters_mat[, j - 1L])
  } else {
    list()
  }
  strata_mat <- matrix(
    data = c(strata_col1, unlist(extra_strata)),
    nrow = n,
    ncol = k
  )

  # Step 5: Build sampsize matrix (k columns)
  sampsize_cols <- vector("list", k)
  for (j in seq_len(k)) {
    parent_j <- if (j == 1L) {
      strata_col1
    } else {
      clusters_mat[, j - 1L]
    }
    cluster_j <- clusters_mat[, j]
    units_per_parent <- tapply(
      cluster_j,
      parent_j,
      function(ids) length(unique(ids))
    )
    sampsize_cols[[j]] <- as.integer(
      units_per_parent[as.character(parent_j)]
    )
  }
  sampsize_mat <- matrix(
    data = unlist(sampsize_cols),
    nrow = n,
    ncol = k
  )

  # Step 6: Build popsize matrix (k columns or NULL)
  if (is.null(vars$fpc)) {
    popsize_mat <- NULL
  } else {
    n_fpc <- length(vars$fpc)
    popsize_cols <- vector("list", k)
    for (j in seq_len(k)) {
      if (j <= n_fpc) {
        fpc_vals <- data[[vars$fpc[[j]]]]
        if (any(fpc_vals > 1, na.rm = TRUE)) {
          # values > 1 are population sizes
          popsize_cols[[j]] <- as.numeric(fpc_vals)
        } else {
          # values in (0,1] are sampling fractions -> convert
          popsize_cols[[j]] <- as.numeric(
            sampsize_cols[[j]] / fpc_vals
          )
        }
      } else {
        # stages beyond n_fpc get Inf (infinite sub-population)
        popsize_cols[[j]] <- rep(Inf, n)
      }
    }
    popsize_mat <- matrix(
      data = unlist(popsize_cols),
      nrow = n,
      ncol = k
    )
  }

  fpcs <- list(sampsize = sampsize_mat, popsize = popsize_mat)

  # Internal consistency assertion
  stopifnot(
    NCOL(clusters_mat) == k,
    NROW(clusters_mat) == n,
    NCOL(strata_mat) == k,
    NROW(strata_mat) == n,
    NCOL(fpcs$sampsize) == k,
    NROW(fpcs$sampsize) == n,
    is.null(fpcs$popsize) ||
      (NCOL(fpcs$popsize) == k && NROW(fpcs$popsize) == n)
  )

  list(
    clusters_mat = clusters_mat,
    strata_mat = strata_mat,
    fpcs = fpcs
  )
}


# ── .get_data_for_select() ────────────────────────────────────────────────────
#
# Returns the data frame to pass to tidyselect::eval_select().
# For data frames, returns x directly. For survey objects, returns x@data.
# Two confirmed call sites: set_sata() and extract_sata().
#' @noRd
.get_data_for_select <- function(x) {
  if (is.data.frame(x)) x else x@data
}

# ── .compute_nonprob_scale() ──────────────────────────────────────────────────
#
# Computes the default variance scale factor for a survey_nonprob design.
# Used by as_survey_nonprob() when scale = NULL.
#
# Args:
#   type: character(1) — one of "bootstrap", "JK1", "JK2", "JKn"
#   R:    integer(1)   — number of replicates
#
# Returns:
#   numeric(1) — the default scale factor for the given type
#     bootstrap: 1/R  (per Wu 2022 / Chen et al. 2021)
#     JK1:       (R-1)/R
#     JK2:       1
#     JKn:       1
#' @noRd
.compute_nonprob_scale <- function(type, R) {
  switch(type, bootstrap = 1 / R, JK1 = (R - 1) / R, JK2 = 1, JKn = 1)
}
