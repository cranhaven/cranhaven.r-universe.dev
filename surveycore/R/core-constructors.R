# R/core-constructors.R
#
# Constructor functions for survey design objects.
#
# Functions defined here (Phase 0, steps 5–7 + Phase 2.5 skeleton):
#   as_survey()           — survey_taylor object (Taylor series linearization)
#   as_survey_replicate() — survey_replicate object (replicate weights)
#   as_survey_twophase()  — survey_twophase object (two-phase sampling)
#   as_survey_nonprob()   — creates a survey_nonprob object (Phase 2.5 skeleton)
#
# This file implements Layer 3 of the 3-layer validator architecture:
#   Layer 1 — S7 class validators      (R/core-classes.R)
#   Layer 2 — reusable validator helpers (R/core-validators.R)
#   Layer 3 — constructor input parsing  (R/core-constructors.R)  <-- this file
#
# Error classes match plans/error-messages.md exactly.
# All cli_abort()/cli_warn() calls include a class= argument.

# ── as_survey ─────────────────────────────────────────────────────────────────

#' Create a Taylor Series Linearization Survey Design
#'
#' Creates a survey design object using Taylor series (linearization) for
#' variance estimation. Supports simple random samples, stratified designs,
#' single- and multi-stage cluster designs, and designs with finite population
#' correction. Uses a tidy-select interface for all design variable arguments.
#'
#' @param data A `data.frame` containing the survey responses. Must have at
#'   least one row and unique column names.
#' @param ids <[`tidy-select`][tidyselect::language]> Cluster (PSU) ID
#'   column(s).
#'   For single-stage: `ids = psu`. For multi-stage: `ids = c(psu, ssu)`.
#'   Omit entirely for simple random sampling.
#' @param probs <[`tidy-select`][tidyselect::language]> Sampling probability
#'   column (a single column, values in (0, 1]). Converted to weights
#'   `= 1/probs` and stored internally. Cannot be used together with `weights`
#'   unless the values are consistent (`weights == 1/probs`).
#' @param weights <[`tidy-select`][tidyselect::language]> Sampling weight
#'   column (a single column, values strictly > 0).
#' @param strata <[`tidy-select`][tidyselect::language]> Stratification
#'   variable column (a single column).
#' @param fpc <[`tidy-select`][tidyselect::language]> Finite population
#'   correction column(s). For single-stage designs, supply one column.
#'   For multi-stage designs, supply one column per stage:
#'   `fpc = c(fpc_stage1, fpc_stage2)`. Each column accepts either total
#'   population size (integer, all > 1) or sampling fraction (numeric,
#'   all in (0, 1]). Cannot contain `NA`. Cannot have more columns than
#'   `ids` stages; fewer is allowed (later stages assume infinite population).
#' @param nest Logical. If `TRUE`, PSU IDs are treated as nested within
#'   strata — i.e., the same ID value in two different strata refers to two
#'   distinct PSUs. Set `nest = TRUE` when PSU IDs are not globally unique
#'   (e.g., NHANES, where PSU IDs restart from 1 in each stratum). Requires
#'   `strata` to be specified. Default `FALSE`.
#' @param calibration A list of calibration data elements, each produced by
#'   [as_caldata()], or `NULL` (default) for no calibration adjustment. When
#'   non-`NULL`, variance estimation applies a Deville-Sarndal GREG projection
#'   that reduces standard errors proportional to the correlation between the
#'   auxiliary variables and the outcome. Equivalent to assigning
#'   `design@calibration <- list(cd)` after construction.
#'
#'   **Known limitations** (not validated at construction time):
#'   - *Weight consistency*: surveycore cannot verify that `cd$w` encodes the
#'     same base weights as the design weight column. Mismatched base weights
#'     produce incorrect variance estimates.
#'   - *Stale calibration after `update_design()`*: changing the weight column
#'     on a calibrated design with [update_design()] makes `@calibration`
#'     stale. Clear `@calibration` manually after any weight column change.
#'
#' @return A `survey_taylor` object.
#'
#' @section Tidy-select:
#' All design variable arguments (`ids`, `probs`, `weights`, `strata`,
#' `fpc`) support tidy-select syntax: bare column names, `c()` to combine
#' multiple columns (multi-stage `ids = c(psu, ssu)`, multi-stage `fpc`),
#' and tidyselect helpers like `starts_with()`. See the Examples section
#' below for runnable demonstrations.
#'
#' @section Simple random sample:
#' When no `ids` or `strata` are specified, the result is a `survey_taylor`
#' object with `NULL` ids and strata — i.e., a simple random sample (SRS).
#' The Taylor variance machinery produces the same estimates as the classical
#' SRS formula `(1 - f) * s^2 / n`. If `weights` and `probs` are also both
#' omitted, uniform weights are assigned and a warning is issued.
#'
#' @section Known limitations:
#' `as_survey()` does not support probability-proportional-to-size (PPS)
#' variance estimation. Taylor series linearization treats all designs as
#' with-replacement, which overestimates (is conservative for) variance in
#' PPS-without-replacement designs. The Yates-Grundy and Brewer/Overton
#' estimators available in [survey::svydesign()] via its `pps` and `variance`
#' arguments are not supported.
#'
#' If your design requires PPS-specific variance estimation, create the design
#' with [survey::svydesign()] and convert it with [from_svydesign()]:
#' ```r
#' d_survey <- survey::svydesign(
#'   ids = ~psu, weights = ~wt, strata = ~stratum,
#'   pps = "brewer", data = mydata
#' )
#' d <- from_svydesign(d_survey)
#' ```
#'
#' @examples
#' # Full NHANES design: stratified cluster with PSU IDs nested within strata
#' d <- as_survey(
#'   nhanes_2017,
#'   ids = sdmvpsu,
#'   weights = wtint2yr,
#'   strata = sdmvstra,
#'   nest = TRUE
#' )
#'
#' # Stratified design without PSU cluster IDs
#' d_strat <- as_survey(nhanes_2017, weights = wtint2yr, strata = sdmvstra)
#'
#' # Blood pressure analysis: filter to exam participants, use MEC weight
#' exam <- nhanes_2017[nhanes_2017$ridstatr == 2, ]
#' d_bp <- as_survey(
#'   exam,
#'   ids = sdmvpsu,
#'   weights = wtmec2yr,
#'   strata = sdmvstra,
#'   nest = TRUE
#' )
#'
#' # c() to combine multiple columns — sketched on a synthetic two-stage frame
#' df <- data.frame(
#'   psu = rep(1:5, each = 4),
#'   ssu = 1:20,
#'   wt = runif(20, 0.5, 2)
#' )
#' d_ms <- as_survey(df, ids = c(psu, ssu), weights = wt)
#'
#' # Tidy-select helpers like starts_with() also work
#' d_h <- as_survey(
#'   gss_2024,
#'   ids = vpsu,
#'   strata = vstrat,
#'   weights = starts_with("wtssn"),
#'   nest = TRUE
#' )
#' @references
#' Deville, J.-C. and Sarndal, C.-E. (1992) Calibration estimators in survey
#' sampling. \emph{Journal of the American Statistical Association}
#' \bold{87}(418), 376--382.
#'
#' Deville, J.-C., Sarndal, C.-E. and Sautory, O. (1993) Generalized raking
#' procedures in survey sampling. \emph{Journal of the American Statistical
#' Association} \bold{88}(423), 1013--1020.
#'
#' Lumley, T. (2004) Analysis of complex survey samples.
#' \emph{Journal of Statistical Software} \bold{9}(1), 1--19.
#'
#' Lumley, T. (2010) \emph{Complex Surveys: A Guide to Analysis Using R}.
#' John Wiley and Sons.
#'
#' Rao, J.N.K., Yung, W. and Hidiroglou, M.A. (2002) Estimating equations for
#' the analysis of survey data using poststratification information.
#' \emph{Sankhya} \bold{64-A}, 22--36.
#'
#' Sarndal, C-E., Swensson, B. and Wretman, J. (1992)
#' \emph{Model Assisted Survey Sampling}. Springer.
#'
#' @seealso
#'   [as_survey_replicate()] for replicate-weight designs,
#'   [as_survey_twophase()] for two-phase designs,
#'   [set_var_label()] to add variable labels
#'
#' @family constructors
#' @export
as_survey <- function(
  data,
  ids = NULL,
  probs = NULL,
  weights = NULL,
  strata = NULL,
  fpc = NULL,
  nest = FALSE,
  calibration = NULL
) {
  call <- match.call()

  # ── Layer 3: data-level validation ─────────────────────────────────────────
  .validate_data_frame(data)

  # ── Capture all quosures at top (before any resolution) ────────────────────
  ids_quo <- rlang::enquo(ids)
  probs_quo <- rlang::enquo(probs)
  weights_quo <- rlang::enquo(weights)
  strata_quo <- rlang::enquo(strata)
  fpc_quo <- rlang::enquo(fpc)

  # ── Resolve tidy-select expressions ─────────────────────────────────────────

  ids_is_null <- rlang::quo_is_null(ids_quo)
  strata_is_null <- rlang::quo_is_null(strata_quo)

  # ids (may select multiple columns for multi-stage designs)
  if (ids_is_null) {
    ids_vars <- NULL
  } else {
    ids_cols <- tidyselect::eval_select(ids_quo, data)
    ids_vars <- names(ids_cols)
  }

  probs_var <- .resolve_single_col(
    probs_quo,
    data,
    "probs",
    class_none = "surveycore_error_weights_not_found",
    class_multi = "surveycore_error_weights_multiple"
  )
  weights_var <- .resolve_single_col(
    weights_quo,
    data,
    "weights",
    class_none = "surveycore_error_weights_not_found",
    class_multi = "surveycore_error_weights_multiple"
  )
  strata_var <- .resolve_single_col(
    strata_quo,
    data,
    "strata",
    class_none = "surveycore_error_strata_not_found",
    class_multi = "surveycore_error_strata_multiple"
  )
  # fpc (may select multiple columns for multi-stage designs)
  if (rlang::quo_is_null(fpc_quo)) {
    fpc_vars <- NULL
  } else {
    fpc_cols <- tidyselect::eval_select(fpc_quo, data)
    if (length(fpc_cols) == 0L) {
      cli::cli_abort(
        c("x" = "{.arg fpc} matched no columns in {.arg data}"),
        class = "surveycore_error_fpc_not_found"
      )
    }
    fpc_vars <- names(fpc_cols)
  }

  # ── Probs / weights reconciliation ─────────────────────────────────────────

  probs_provided <- FALSE

  if (!is.null(probs_var) && !is.null(weights_var)) {
    # Both specified — check for consistency
    computed_wt <- 1 / data[[probs_var]]
    provided_wt <- data[[weights_var]]
    if (!isTRUE(all.equal(computed_wt, provided_wt, tolerance = 1e-6))) {
      cli::cli_abort(
        c(
          "x" = paste0(
            "Cannot specify both {.arg probs} and {.arg weights} ",
            "with inconsistent values. ",
            "{.arg weights} should equal 1 / {.arg probs}"
          )
        ),
        class = "surveycore_error_probs_weights_conflict"
      )
    }
    # Consistent — use the provided weights column, note probs was consistent
    cli::cli_inform(
      paste0(
        "Using {.arg weights}; provided {.arg probs} is consistent ",
        "(weights = 1/probs)"
      ),
      class = "surveycore_inform_probs_weights_consistent"
    )
    probs_provided <- TRUE
  } else if (!is.null(probs_var) && is.null(weights_var)) {
    # Only probs provided — convert to weights
    weights_var <- .SURVEYCORE_WT_COL
    data[[weights_var]] <- 1 / data[[probs_var]]
    probs_provided <- TRUE
  } else if (is.null(probs_var) && is.null(weights_var)) {
    # Neither probs nor weights — SRS fallback
    # Warning 7: no weights or probs (SRS fallback); text varies by ids presence
    weights_var <- .SURVEYCORE_WT_COL
    data[[weights_var]] <- rep(1L, nrow(data))
    probs_provided <- FALSE

    if (!is.null(ids_vars)) {
      cli::cli_warn(
        c(
          "!" = "No weights provided.",
          "i" = paste0(
            "Treating as equal-probability sampling within clusters ",
            "(unknown population size)."
          ),
          "i" = paste0(
            "Population totals will equal sample totals, not ",
            "estimated population totals."
          )
        ),
        class = "surveycore_warning_srs_no_weights"
      )
    } else {
      cli::cli_warn(
        c(
          "!" = "No weights or population size provided.",
          "i" = paste0(
            "Treating as equal-probability SRS with unknown ",
            "population size."
          ),
          "i" = paste0(
            "Population totals will equal sample totals, not ",
            "estimated population totals."
          )
        ),
        class = "surveycore_warning_srs_no_weights"
      )
    }
  }

  # ── Business-rule validations ───────────────────────────────────────────────

  # Error 15: nest = TRUE requires strata
  if (isTRUE(nest) && is.null(strata_var)) {
    cli::cli_abort(
      c("x" = "{.arg nest = TRUE} requires {.arg strata} to be specified"),
      class = "surveycore_error_nest_without_strata"
    )
  }

  # Validate weights column (all zero / non-positive check via Layer 2)
  .validate_weights(weights_var, data)

  # Validate design variable columns exist and are atomic
  .validate_design_vars(c(ids_vars, strata_var), data)

  # ── Multi-stage FPC validation ──────────────────────────────────────────────

  if (!is.null(fpc_vars)) {
    n_fpc <- length(fpc_vars)
    n_ids <- if (is.null(ids_vars)) 1L else length(ids_vars)

    # Error 88: fpc has more columns than ID stages
    if (n_fpc > n_ids) {
      cli::cli_abort(
        c(
          "x" = paste0(
            "{.arg fpc} selected {n_fpc} column(s) but ",
            "{.arg ids} has only {n_ids} stage(s)."
          ),
          "i" = paste0(
            "FPC columns must correspond 1-to-1 with ID stages. ",
            "Supply at most {n_ids} FPC column(s)."
          )
        ),
        class = "surveycore_error_fpc_too_many_stages"
      )
    }

    # Warning 89: fpc has fewer columns than ID stages
    if (n_fpc < n_ids) {
      cli::cli_warn(
        c(
          "!" = paste0(
            "{.arg fpc} has {n_fpc} column(s) but ",
            "{.arg ids} has {n_ids} stage(s)."
          ),
          "i" = paste0(
            "Later stages assume sampling from an infinite ",
            "population (no FPC)."
          )
        ),
        class = "surveycore_warning_fpc_partial_stages"
      )
    }

    # Per-column FPC validation loop
    for (j in seq_along(fpc_vars)) {
      fpc_var_j <- fpc_vars[[j]]
      fpc_col_j <- data[[fpc_var_j]]

      # FPC check: NA values (reuse Layer 2 helper for each column)
      .validate_fpc(fpc_var_j, data)

      # FPC check: non-positive values (row 57)
      n_bad <- sum(fpc_col_j <= 0, na.rm = TRUE)
      if (n_bad > 0L) {
        cli::cli_abort(
          c(
            "x" = paste0(
              "{.arg fpc} column {.field {fpc_var_j}} has {n_bad} ",
              "non-positive value(s). FPC values must be > 0."
            ),
            "i" = paste0(
              "FPC must be either population sizes (> 1) or ",
              "sampling fractions (0 < f \u2264 1)."
            )
          ),
          class = "surveycore_error_fpc_nonpositive"
        )
      }

      # Determine if this column is population size or fraction
      has_above_one <- any(fpc_col_j > 1, na.rm = TRUE)
      has_le_one <- any(fpc_col_j <= 1, na.rm = TRUE)

      # FPC check: ambiguous (mixes > 1 and <= 1) — row 58
      if (has_above_one && has_le_one) {
        cli::cli_abort(
          c(
            "x" = paste0(
              "{.arg fpc} column {.field {fpc_var_j}} mixes ",
              "values > 1 (population sizes) and values ",
              "\u2264 1 (sampling fractions). All FPC values ",
              "must be consistently one type."
            ),
            "i" = paste0(
              "Use all values > 1 for population sizes, or ",
              "all values in (0, 1] for sampling fractions."
            )
          ),
          class = "surveycore_error_fpc_ambiguous"
        )
      }

      # Stage-j-aware checks: parent cluster grouping
      if (has_above_one) {
        # Population size type: check within parent clusters
        # For stage 1: group by strata (if present) or treat as one group
        # For stage j>1: group by the (j-1)th ID column
        if (j == 1L) {
          parent_col <- if (!is.null(strata_var)) {
            data[[strata_var]]
          } else {
            rep(1L, nrow(data))
          }
        } else {
          parent_col <- data[[ids_vars[[j - 1L]]]]
        }

        # Error 90: FPC pop size < cluster count within parent
        parent_groups <- split(
          seq_len(nrow(data)),
          parent_col
        )
        n_bad_parents <- 0L
        for (grp_rows in parent_groups) {
          fpc_val <- fpc_col_j[grp_rows[[1L]]]
          # Count clusters at this stage within this parent
          if (j <= length(ids_vars)) {
            n_clusters <- length(
              unique(data[[ids_vars[[j]]]][grp_rows])
            )
          } else {
            n_clusters <- length(grp_rows)
          }
          if (fpc_val < n_clusters) {
            n_bad_parents <- n_bad_parents + 1L
          }
        }

        if (n_bad_parents > 0L) {
          cli::cli_abort(
            c(
              "x" = paste0(
                "Stage-{j} FPC column {.field {fpc_var_j}} has ",
                "population sizes smaller than the observed ",
                "cluster count in {n_bad_parents} parent ",
                "group(s)."
              ),
              "i" = paste0(
                "Population size must be >= the number of ",
                "sampled units within each parent cluster."
              )
            ),
            class = "surveycore_error_fpc_smaller_than_n"
          )
        }
      } else {
        # Fraction type: check constancy within parent clusters
        if (j == 1L) {
          parent_col <- if (!is.null(strata_var)) {
            data[[strata_var]]
          } else {
            rep(1L, nrow(data))
          }
        } else {
          parent_col <- data[[ids_vars[[j - 1L]]]]
        }

        parent_groups <- split(
          seq_len(nrow(data)),
          parent_col
        )
        any_not_constant <- FALSE
        for (grp_rows in parent_groups) {
          vals <- fpc_col_j[grp_rows]
          if (length(unique(vals)) > 1L) {
            any_not_constant <- TRUE
            break
          }
        }

        if (any_not_constant) {
          cli::cli_abort(
            c(
              "x" = paste0(
                "Stage-{j} FPC column {.field {fpc_var_j}} is ",
                "not constant within parent clusters."
              ),
              "i" = paste0(
                "FPC fractions must be the same for all units ",
                "within each parent cluster."
              )
            ),
            class = "surveycore_error_fpc_not_constant"
          )
        }
      }
    }
  }

  # Warning 12: strata has only one unique value
  if (!is.null(strata_var)) {
    n_unique_strata <- length(unique(data[[strata_var]]))
    if (n_unique_strata == 1L) {
      cli::cli_warn(
        c(
          "!" = paste0(
            "{.arg strata} ({.field {strata_var}}) has only 1 unique ",
            "value \u2014 stratification has no effect"
          )
        ),
        class = "surveycore_warning_single_stratum"
      )
    }
  }

  # ── Build @variables list ───────────────────────────────────────────────────

  variables <- list(
    ids = ids_vars,
    weights = weights_var,
    strata = strata_var,
    fpc = fpc_vars,
    nest = isTRUE(nest),
    probs_provided = probs_provided,
    visible_vars = NULL
  )

  # ── Extract haven-style metadata ────────────────────────────────────────────

  metadata <- .extract_haven_metadata(data)
  metadata <- .promote_weighting_history(data, metadata)

  # ── Validate calibration argument ──────────────────────────────────────────

  .validate_calibration_arg(calibration, nrow(data))

  # ── Construct and return survey_taylor object ───────────────────────────────

  survey_taylor(
    data = data,
    metadata = metadata,
    variables = variables,
    calibration = calibration,
    call = call
  )
}


# ── as_survey_replicate ───────────────────────────────────────────────────────

#' Create a Replicate Weights Survey Design
#'
#' Creates a survey design object using replicate weights for variance
#' estimation. Supports all common replicate methods: jackknife (JK1, JK2,
#' JKn), balanced repeated replication (BRR, Fay), bootstrap, ACS,
#' successive-difference, and user-defined types. Uses a tidy-select
#' interface for weight and replicate-weight columns.
#'
#' @param data A `data.frame` containing the survey responses. Must have at
#'   least one row and unique column names.
#' @param weights <[`tidy-select`][tidyselect::language]> Sampling weight
#'   column (a single column, values strictly > 0). Required.
#' @param repweights <[`tidy-select`][tidyselect::language]> Replicate weight
#'   columns. Must select at least one column. Supports tidy-select helpers
#'   (e.g., `starts_with("repwt")`). Required.
#' @param type Character. Replicate weight method. One of `"JK1"` (delete-1
#'   jackknife), `"JK2"` (delete-1 jackknife, stratified), `"JKn"` (delete-1
#'   jackknife with varying replication counts), `"BRR"` (balanced repeated
#'   replication), `"Fay"` (Fay's method, a modified BRR), `"bootstrap"`,
#'   `"ACS"` (used in American Community Survey), `"successive-difference"`,
#'   or `"other"` (user-specified scale). Case-sensitive.
#' @param scale Numeric. Scaling factor applied to the replicate variance
#'   formula. If `NULL` (default), computed automatically from `type` and
#'   the number of replicates `R`: `(R-1)/R` for `"JK1"`, `"JK2"`, and
#'   `"JKn"`; `1/R` for `"BRR"`, `"Fay"`, `"bootstrap"`, and `"ACS"`;
#'   `2/R` for `"successive-difference"`; `1` for `"other"`.
#' @param rscales Numeric vector of replicate-specific scaling factors, or
#'   `NULL`. If provided, must have the same length as the number of
#'   replicate weight columns selected by `repweights`.
#' @param fpc <[`tidy-select`][tidyselect::language]> Finite population
#'   correction column (a single column). Used by some replicate methods to
#'   adjust the variance estimator. `NULL` means no FPC correction.
#' @param fpctype Character. How `fpc` is interpreted: `"fraction"` (sampling
#'   fraction, 0–1) or `"correction"` (multiplier for the replicate variance).
#'   Default `"fraction"`. Case-sensitive.
#' @param mse Logical. If `TRUE` (default), use mean-squared-error estimates
#'   (subtract the full-sample estimate rather than the mean replicate estimate
#'   when computing variance). Recommended for most designs.
#' @param calibration A list of calibration data elements, each produced by
#'   [as_caldata()], or `NULL` (default). Stored at `@calibration` for
#'   provenance and reproducibility. **Not used in variance estimation**: the
#'   replicate variance estimator ignores `@calibration` entirely — calibration
#'   is already encoded in the replicate weights.
#'
#'   **Known limitations** (not validated at construction time):
#'   - *Weight consistency*: surveycore cannot verify that `cd$w` encodes the
#'     same base weights as the design weight column.
#'   - *Stale calibration after `update_design()`*: changing the weight column
#'     makes `@calibration` stale; clear it manually.
#'
#' @return A `survey_replicate` object.
#'
#' @section Tidy-select:
#' Both `weights` and `repweights` support tidy-select syntax:
#' ```r
#' # Bare name for weights
#' as_survey_replicate(
#'   df, weights = wt, repweights = starts_with("repwt"), type = "BRR"
#' )
#' # c() for explicit replicate columns
#' as_survey_replicate(
#'   df, weights = wt, repweights = c(rep1, rep2, rep3), type = "JK1"
#' )
#' ```
#'
#' @section Replicate weight matrix:
#' The replicate weight matrix is **not stored** in the object. Only the
#' column names are stored in `@variables$repweights`. Variance estimation
#' computes the matrix on demand:
#' `as.matrix(design@data[, design@variables$repweights])`.
#'
#' @section Memory usage:
#' Each call to an estimation function (e.g., [get_means()], [get_totals()])
#' materialises the full replicate weight matrix from the data frame. For large
#' designs (e.g., ACS PUMS with 500k+ rows × 80 replicates), this is roughly
#' `nrow * n_replicates * 8` bytes per call (~363 MB for ACS Wyoming × 80).
#' If you are estimating many variables, this is repeated for each call.
#' This behaviour matches the `survey` package reference implementation.
#'
#' @examples
#' # ACS PUMS Wyoming: 80 successive-difference replicate weights
#' d_acs <- as_survey_replicate(
#'   acs_pums_wy,
#'   weights = pwgtp,
#'   repweights = pwgtp1:pwgtp80,
#'   type = "successive-difference"
#' )
#'
#' # Explicit replicate columns using c()
#' d_sub <- as_survey_replicate(
#'   acs_pums_wy,
#'   weights = pwgtp,
#'   repweights = c(pwgtp1, pwgtp2, pwgtp3, pwgtp4),
#'   type = "JK1"
#' )
#' @references
#' Canty, A.J. and Davison, A.C. (1999) Resampling-based variance estimation
#' for labour force surveys. \emph{The Statistician} \bold{48}(3), 379--391.
#'
#' Deville, J.-C. and Sarndal, C.-E. (1992) Calibration estimators in survey
#' sampling. \emph{Journal of the American Statistical Association}
#' \bold{87}(418), 376--382.
#'
#' Deville, J.-C., Sarndal, C.-E. and Sautory, O. (1993) Generalized raking
#' procedures in survey sampling. \emph{Journal of the American Statistical
#' Association} \bold{88}(423), 1013--1020.
#'
#' Judkins, D.R. (1990) Fay's method for variance estimation.
#' \emph{Journal of the American Statistical Association}
#' \bold{85}(410), 895--904.
#'
#' Rao, J.N.K., Wu, C.F.J. and Yue, K. (1992) Some recent work on resampling
#' methods for complex surveys. \emph{Survey Methodology} \bold{18}(2),
#' 209--217.
#'
#' Shao, J. and Tu, D. (1995) \emph{The Jackknife and Bootstrap}. Springer.
#'
#' @seealso
#'   [as_survey()] for Taylor series designs,
#'   [as_survey_twophase()] for two-phase designs,
#'   [set_var_label()] to add variable labels
#'
#' @family constructors
#' @export
as_survey_replicate <- function(
  data,
  weights,
  repweights,
  type = c(
    "JK1",
    "JK2",
    "JKn",
    "BRR",
    "Fay",
    "bootstrap",
    "ACS",
    "successive-difference",
    "other"
  ),
  scale = NULL,
  rscales = NULL,
  fpc = NULL,
  fpctype = c("fraction", "correction"),
  mse = TRUE,
  calibration = NULL
) {
  call <- match.call()
  type <- match.arg(type)
  fpctype <- match.arg(fpctype)

  # ── Layer 3: data-level validation ─────────────────────────────────────────
  .validate_data_frame(data)

  # ── Resolve tidy-select expressions ────────────────────────────────────────

  # weights (must select exactly one column; R function signature already
  # requires it — no default, so missing arg is caught by R before we run)
  weights_var <- .resolve_single_col(
    rlang::enquo(weights),
    data,
    "weights",
    class_none = "surveycore_error_weights_not_found",
    class_multi = "surveycore_error_weights_multiple"
  )

  # repweights (must select at least one column)
  repweights_quo <- rlang::enquo(repweights)
  repweights_cols <- tidyselect::eval_select(repweights_quo, data)
  if (length(repweights_cols) == 0L) {
    cli::cli_abort(
      c("x" = "{.arg repweights} must select at least one column"),
      class = "surveycore_error_repweights_empty"
    )
  }
  repweights_vars <- names(repweights_cols)
  n_rep <- length(repweights_vars)

  fpc_var <- .resolve_single_col(
    rlang::enquo(fpc),
    data,
    "fpc",
    class_none = "surveycore_error_fpc_not_found",
    class_multi = "surveycore_error_fpc_multiple"
  )

  # ── Business-rule validations ───────────────────────────────────────────────

  # Validate weights column (all zero / non-positive check via Layer 2)
  .validate_weights(weights_var, data)

  # Validate repweights columns are numeric (Layer 2)
  .validate_repweights(repweights_vars, data)

  # Validate fpc column if provided (no NAs allowed)
  .validate_fpc(fpc_var, data)

  # Error 17: rscales length must match number of replicates (Layer 2)
  .validate_rscales(rscales, n_rep)

  # ── Compute default scale based on type and n_rep ───────────────────────────

  if (is.null(scale)) {
    scale <- switch(
      type,
      JK1 = (n_rep - 1L) / n_rep,
      JK2 = (n_rep - 1L) / n_rep,
      JKn = (n_rep - 1L) / n_rep,
      # BRR variance formula: (1/R) * sum((theta_r - theta)^2). The survey
      # package hardcodes this same formula internally (scale= is ignored for
      # BRR). Oracle test in test-variance-replicate.R verifies agreement.
      BRR = 1 / n_rep,
      Fay = 1 / n_rep,
      bootstrap = 1 / n_rep,
      ACS = 1 / n_rep,
      `successive-difference` = 2 / n_rep,
      other = 1
    )
  }

  # ── Build @variables list ───────────────────────────────────────────────────

  variables <- list(
    weights = weights_var,
    repweights = repweights_vars,
    type = type,
    scale = scale,
    rscales = rscales,
    fpc = fpc_var,
    fpctype = fpctype,
    mse = isTRUE(mse),
    visible_vars = NULL
  )

  # ── Extract haven-style metadata ────────────────────────────────────────────

  metadata <- .extract_haven_metadata(data)
  metadata <- .promote_weighting_history(data, metadata)

  # ── Validate calibration argument ──────────────────────────────────────────

  .validate_calibration_arg(calibration, nrow(data))

  # ── Construct and return survey_replicate object ────────────────────────────

  survey_replicate(
    data = data,
    metadata = metadata,
    variables = variables,
    calibration = calibration,
    call = call
  )
}


# ── as_survey_twophase ────────────────────────────────────────────────────────

#' Create a Two-Phase Survey Design
#'
#' Creates a two-phase (double) sampling design from an existing
#' `survey_taylor` Phase 1 object. Phase 1 covers all rows; Phase 2 is a
#' strict subset indicated by a logical column. Uses a tidy-select interface
#' for all Phase 2 design variable arguments.
#'
#' @param phase1 A survey design object (inheriting from `survey_base`)
#'   representing the Phase 1 design. Accepts `survey_taylor` or
#'   `survey_replicate` objects.
#'   Its `@data` must contain ALL rows from both phases, plus a logical
#'   indicator column for Phase 2 membership. Create with [as_survey()]
#'   or [as_survey_replicate()].
#' @param ids2 <[`tidy-select`][tidyselect::language]> Phase 2 cluster ID
#'   column(s). For single-stage Phase 2: `ids2 = psu2`. For multi-stage:
#'   `ids2 = c(psu2, ssu2)`. Omit if Phase 2 has no within-stratum
#'   clustering.
#' @param strata2 <[`tidy-select`][tidyselect::language]> Phase 2
#'   stratification column (a single column). Optional.
#' @param probs2 <[`tidy-select`][tidyselect::language]> Phase 2 inclusion
#'   probability column (a single column, values in (0, 1]). Optional.
#' @param fpc2 <[`tidy-select`][tidyselect::language]> Phase 2 finite
#'   population correction column (a single column). Optional.
#' @param subset <[`tidy-select`][tidyselect::language]> Single logical column
#'   in `phase1@data`. `TRUE` = row selected into Phase 2;
#'   `FALSE` = Phase 1 only. Required. Must contain both `TRUE` and `FALSE`
#'   values (non-degenerate).
#' @param method Character. Variance estimation method for combining Phase 1
#'   and Phase 2 variability. One of `"full"` (default), `"approx"`, or
#'   `"simple"`. Case-sensitive. See Details.
#'
#' @details
#' ## Variance methods
#'
#' * `"full"` — Full two-phase variance formula. Accounts for variability in
#'   both phases. Requires Phase 2 design information (`probs2`, `ids2`,
#'   `strata2`) when Phase 2 is not a simple random subsample. If none of
#'   these are provided, an error is raised.
#'
#' * `"approx"` — Approximation that ignores Phase 1 sampling variability.
#'   Faster but less accurate than `"full"` when the Phase 1 sampling fraction
#'   is non-negligible.
#'
#' * `"simple"` — Treats Phase 2 as a single-phase design, ignoring Phase 1.
#'   Only valid when Phase 1 is a census (no sampling). Issues a warning when
#'   Phase 1 has PSU cluster variables, because this understates variance for
#'   clustered designs.
#'
#' @return A `survey_twophase` object.
#'
#' @examples
#' # Minimal two-phase design: Phase 1 = full cohort, Phase 2 = random subset
#' df <- data.frame(
#'   id = 1:20,
#'   wt = rep(2, 20),
#'   in_phase2 = c(rep(TRUE, 10), rep(FALSE, 10)),
#'   y = rnorm(20)
#' )
#' phase1 <- as_survey(df, ids = id, weights = wt)
#' d2 <- as_survey_twophase(phase1, subset = in_phase2)
#'
#' # With Phase 2 stratification and inclusion probabilities
#' df2 <- data.frame(
#'   id = 1:30,
#'   wt = rep(3, 30),
#'   in_phase2 = c(rep(TRUE, 15), rep(FALSE, 15)),
#'   arm = rep(c("A", "B", "C"), 10),
#'   subsamprate = rep(c(0.5, 0.7, 0.3), 10),
#'   y = rnorm(30)
#' )
#' phase1b <- as_survey(df2, ids = id, weights = wt)
#' d2b <- as_survey_twophase(
#'   phase1b,
#'   strata2 = arm,
#'   probs2 = subsamprate,
#'   subset = in_phase2,
#'   method = "full"
#' )
#' @references
#' Sarndal, C-E., Swensson, B. and Wretman, J. (1992)
#' \emph{Model Assisted Survey Sampling}. Springer.
#'
#' Breslow, N.E. and Chatterjee, N. (1999) Design and analysis of two-phase
#' studies with binary outcome applied to Wilms tumour prognosis.
#' \emph{Applied Statistics} \bold{48}, 457--468.
#'
#' Breslow, N., Lumley, T., Ballantyne, C.M., Chambless, L.E. and Kulick, M.
#' (2009) Improved Horvitz-Thompson estimation of model parameters from
#' two-phase stratified samples: applications in epidemiology.
#' \emph{Statistics in Biosciences}.
#' \doi{10.1007/s12561-009-9001-6}
#'
#' @seealso
#'   [as_survey()] for Taylor series designs,
#'   [as_survey_replicate()] for replicate-weight designs
#'
#' @family constructors
#' @export
as_survey_twophase <- function(
  phase1,
  ids2 = NULL,
  strata2 = NULL,
  probs2 = NULL,
  fpc2 = NULL,
  subset,
  method = c("full", "approx", "simple")
) {
  call <- match.call()
  method <- match.arg(method)

  # ── Error 19: phase1 must be a survey design object ─────────────────────────

  if (!S7::S7_inherits(phase1, survey_base)) {
    cli::cli_abort(
      c(
        "x" = paste0(
          "{.arg phase1} must be a survey design object ",
          "({.cls survey_base}), not {.cls {class(phase1)[[1L]]}}."
        ),
        "i" = paste0(
          "Create it first with {.fn as_survey} or ",
          "{.fn as_survey_replicate}."
        )
      ),
      class = "surveycore_error_phase1_class"
    )
  }

  data <- phase1@data

  # ── Error 20: subset is required ────────────────────────────────────────────

  subset_quo <- rlang::enquo(subset)
  if (rlang::quo_is_missing(subset_quo)) {
    cli::cli_abort(
      c(
        "x" = paste0(
          "{.arg subset} is required: a logical column indicating ",
          "Phase 2 membership"
        )
      ),
      class = "surveycore_error_subset_missing"
    )
  }

  subset_cols <- tidyselect::eval_select(subset_quo, data)

  # ── Error 21: subset must select exactly one column ─────────────────────────

  if (length(subset_cols) == 0L) {
    cli::cli_abort(
      c("x" = "{.arg subset} matched no columns in {.arg phase1}"),
      class = "surveycore_error_subset_missing"
    )
  }
  if (length(subset_cols) > 1L) {
    cli::cli_abort(
      c(
        "x" = paste0(
          "{.arg subset} must select exactly one column, not ",
          "{length(subset_cols)}"
        )
      ),
      class = "surveycore_error_subset_multiple"
    )
  }
  subset_var <- names(subset_cols)

  # ── Error 22: subset column must be logical ─────────────────────────────────

  if (!is.logical(data[[subset_var]])) {
    cli::cli_abort(
      c(
        "x" = paste0(
          "{.arg subset} column {.field {subset_var}} must be logical, ",
          "not {.cls {class(data[[subset_var]])}}"
        )
      ),
      class = "surveycore_error_subset_not_logical"
    )
  }

  # ── Error 23: subset must be non-degenerate ─────────────────────────────────

  subset_vals <- data[[subset_var]]

  # Error: NAs in subset column — phase 2 membership must be fully observed.
  n_na <- sum(is.na(subset_vals))
  if (n_na > 0L) {
    cli::cli_abort(
      c(
        "x" = paste0(
          "{.arg subset} column {.field {subset_var}} contains {n_na} ",
          "NA value(s)."
        ),
        "i" = paste0(
          "The phase 2 membership indicator must be fully observed ",
          "for all phase 1 units."
        ),
        "v" = paste0(
          "Remove rows with missing {.arg subset} values before calling ",
          "{.fn as_survey_twophase}."
        )
      ),
      class = "surveycore_error_subset_na"
    )
  }

  n_true <- sum(subset_vals, na.rm = TRUE)
  n_false <- sum(!subset_vals, na.rm = TRUE)
  if (n_true == 0L || n_false == 0L) {
    cli::cli_abort(
      c(
        "x" = paste0(
          "{.arg subset} column {.field {subset_var}} must contain both ",
          "TRUE and FALSE values (non-NA). ",
          "Found {n_true} TRUE and {n_false} FALSE (non-NA) value(s)."
        )
      ),
      class = "surveycore_error_subset_degenerate"
    )
  }

  # ── Resolve optional Phase 2 design columns via tidy-select ─────────────────

  # ids2 (may select multiple columns for multi-stage Phase 2 clustering)
  ids2_quo <- rlang::enquo(ids2)
  if (rlang::quo_is_null(ids2_quo)) {
    ids2_vars <- NULL
  } else {
    ids2_cols <- tidyselect::eval_select(ids2_quo, data)
    ids2_vars <- if (length(ids2_cols) == 0L) NULL else names(ids2_cols)
  }

  strata2_var <- .resolve_single_col(
    rlang::enquo(strata2),
    data,
    "strata2",
    class_none = "surveycore_error_strata_not_found",
    class_multi = "surveycore_error_strata_multiple"
  )
  probs2_var <- .resolve_single_col(
    rlang::enquo(probs2),
    data,
    "probs2",
    class_none = "surveycore_error_weights_not_found",
    class_multi = "surveycore_error_weights_multiple"
  )
  fpc2_var <- .resolve_single_col(
    rlang::enquo(fpc2),
    data,
    "fpc2",
    class_none = "surveycore_error_fpc_not_found",
    class_multi = "surveycore_error_fpc_multiple"
  )

  # ── Warning 24: method = "simple" with clustered Phase 1 ────────────────────

  if (method == "simple" && !is.null(phase1@variables$ids)) {
    cli::cli_warn(
      c(
        "!" = paste0(
          '{.code method = "simple"} ignores the Phase 1 cluster design ',
          "(PSUs: {.field {phase1@variables$ids}}). ",
          "This understates variance. ",
          'Use {.code method = "full"} or {.code method = "approx"}.'
        )
      ),
      class = "surveycore_warning_simple_clustered"
    )
  }

  # ── Build @variables list ───────────────────────────────────────────────────

  phase2_vars <- list(
    ids = ids2_vars,
    strata = strata2_var,
    probs = probs2_var,
    fpc = fpc2_var
  )

  variables <- list(
    phase1 = phase1@variables,
    phase2 = phase2_vars,
    subset = subset_var,
    method = method,
    visible_vars = NULL
  )

  # ── Inherit metadata from phase1 ────────────────────────────────────────────

  metadata <- phase1@metadata

  # ── Construct and return survey_twophase object ─────────────────────────────

  survey_twophase(
    data = data,
    metadata = metadata,
    variables = variables,
    call = call
  )
}


# ── .is_stratified_jk() ──────────────────────────────────────────────────────
#
# Returns TRUE when `type` is a stratified jackknife variant (JK2 or JKn).
# Used by as_survey_nonprob() to gate the rscales requirement check.
# One confirmed call site: as_survey_nonprob().
.is_stratified_jk <- function(type) type %in% c("JK2", "JKn")


# ── as_survey_nonprob ───────────────────────────────────────────────────────

#' Create a Non-probability Survey Design
#'
#' Creates a survey design object for non-probability samples (e.g., online
#' panels, quota samples, volunteer panels). Accepts pre-computed calibration
#' weights (including raking and post-stratification) or inverse probability
#' weighting (IPW) pseudo-weights.
#'
#' @section When to use:
#' Use `as_survey_nonprob()` instead of [as_survey()] when:
#' \itemize{
#'   \item Your data comes from a non-probability sample (online panel, quota
#'     sample, MTurk/Prolific, etc.)
#'   \item You have calibration or raking weights but no probability sampling
#'     design structure (no PSU IDs, strata, etc.)
#'   \item You want to explicitly record the provenance of your calibration
#'     weights for reproducibility
#' }
#'
#' If your data comes from a probability sample with known design structure,
#' use [as_survey()], [as_survey_replicate()], or [as_survey_twophase()]
#' instead.
#'
#' @section Variance estimation:
#' Two modes are available, depending on whether `repweights` is supplied:
#' \describe{
#'   \item{**SRS approximation** (`repweights = NULL`, the default)}{Standard
#'     errors treat the calibrated weights as fixed and assume simple random
#'     sampling. This is a model-assisted approximation that understates
#'     calibration uncertainty. Use this mode only when replicate weights are
#'     unavailable; interpret standard errors with caution (Valliant 2020;
#'     Elliott and Valliant 2017).}
#'   \item{**Bootstrap variance** (`repweights` supplied)}{Each replicate weight
#'     column must contain calibrated weights re-estimated on one bootstrap
#'     draw (i.e., raking or post-stratification was re-applied within each
#'     replicate). This propagates calibration uncertainty into the variance
#'     estimate and is the recommended approach (Chrostowski et al. 2025;
#'     Kolenikov 2014).}
#' }
#' See `vignette("creating-survey-objects")` for guidance on choosing between
#' these modes and on the limitations of SRS-based variance for calibrated
#' non-probability samples.
#'
#' @param data A `data.frame` containing the survey responses with
#'   pre-computed calibration weights. Must have at least one row and
#'   unique column names.
#' @param weights <[`tidy-select`][tidyselect::language]> Calibration weight
#'   column (a single column, values strictly > 0). Typically produced by
#'   an external raking function (e.g., `anesrake::anesrake()`) or a
#'   \pkg{surveywts} calibration function.
#' @param repweights <[`tidy-select`][tidyselect::language]> Replicate weight
#'   columns (bootstrap or jackknife; at least 2). Each column must be numeric
#'   and represents one set of calibrated weights re-estimated on one replicate
#'   draw (calibration already applied within each replicate). Supply `NULL`
#'   (the default) to use SRS-based variance approximation. See `type` for
#'   supported replicate schemes.
#' @param type Character scalar. Replicate variance type. When
#'   \code{repweights = NULL}, this argument is ignored. Case-sensitive.
#'   Valid values:
#'   \describe{
#'     \item{\code{"bootstrap"}}{Bootstrap variance. Default scale: \code{1/R}.
#'       Default value for \code{type}.}
#'     \item{\code{"JK1"}}{Delete-one jackknife for unclustered nonprob designs.
#'       Default scale: \code{(R-1)/R}. Appropriate when each unit is its own
#'       replication unit. For clustered designs, use \code{"JK2"} or
#'       \code{"JKn"} with explicit \code{rscales}.}
#'     \item{\code{"jackknife"}}{Alias for \code{"JK1"}. Normalized to
#'       \code{"JK1"} before storage — the stored value is always
#'       \code{"JK1"}, never \code{"jackknife"}.}
#'     \item{\code{"JK2"}}{Stratified jackknife. Default scale: \code{1}.
#'       Requires explicit \code{rscales} (stratum-specific scale factors of
#'       the form \code{(n_h - 1) / n_h}).}
#'     \item{\code{"JKn"}}{Equivalent to \code{"JK2"} for stratified nonprob
#'       designs. Default scale: \code{1}. Requires explicit \code{rscales}.}
#'   }
#' @param scale Numeric scalar. Scaling factor for the replicate variance
#'   formula. Default `NULL`, which sets `scale = 1 / R` (where `R` is the
#'   number of replicate columns). Note: this default differs from
#'   [as_survey_replicate()], which uses type-specific defaults.
#' @param rscales Numeric vector of length `R`. Per-replicate scale factors.
#'   All values must be non-negative and non-NA. Default `NULL`, which sets
#'   `rscales = rep(1, R)`.
#' @param mse Logical. If `TRUE` (the default), the mean-squared-error form
#'   of the variance estimator is used: `(1/R) * sum((theta_r - theta)^2)`.
#'   If `FALSE`, the centered form is used instead. Default `TRUE`. Note:
#'   this default differs from [as_survey_replicate()] — `mse = TRUE` is
#'   the appropriate default for bootstrap replicates from calibrated
#'   non-probability samples (Wu 2022).
#' @param reference_sample Optional. A [survey_taylor] object representing the
#'   probability-based reference sample used to estimate propensity scores or
#'   calibration targets. Stored in `@reference_sample` for reproducibility.
#'   Supply `NULL` (the default) when no reference sample is available.
#' @param calibration Optional. A calibration provenance object returned by
#'   a \pkg{surveywts} weighting function. Stored in `@calibration` for
#'   reproducibility only — it is not used in variance estimation (unlike
#'   [as_survey()] where `@calibration` drives GREG variance correction).
#'   When `repweights` is also supplied, two consistency checks are applied:
#'   for `type = "bootstrap"`, `calibration$bootstrap` must be `TRUE`; for
#'   all types, `calibration$R` must equal the number of replicate columns
#'   when `calibration$R` is non-`NULL`. Supply `NULL` (the default) when no
#'   provenance metadata is available.
#'
#' @return A `survey_nonprob` object.
#'
#' @details
#' Unlike probability samples, non-probability samples have no design weights
#' derived from known selection probabilities, which means estimates carry
#' additional uncertainty not captured by standard design-based variance
#' formulas. Per Elliott and Valliant (2017), Valliant, Dever, and Kreuter
#' (2018), and Brick (2015), bootstrap or jackknife replicate weights are the
#' recommended approach for variance estimation — they propagate calibration
#' uncertainty into standard errors. Note, however, that replicate variance
#' addresses calibration uncertainty only; it does not resolve uncertainty about
#' the selection mechanism itself, which requires untestable modeling
#' assumptions about the relationship between sample membership and the survey
#' variables of interest. Without replicate weights, standard errors use a
#' model-assisted SRS approximation that systematically underestimates variance
#' for non-probability samples.
#'
#' When `repweights` is supplied, the variance estimator uses the replicate
#' formula: `V = scale * sum(rscales * (theta_r - theta)^2)`. For bootstrap
#' replicates (`type = "bootstrap"`), the default `scale = 1/R` follows Wu
#' (2022) and Chen et al. (2021). For jackknife replicates (`type = "JK1"`,
#' `"JK2"`, or `"JKn"`), scale and rscales follow the standard jackknife
#' variance conventions; see `type` for defaults.
#'
#' When `repweights = NULL`, standard errors use an SRS approximation (treating
#' each observation as its own PSU). This understates calibration uncertainty;
#' see `vignette("creating-survey-objects")` for details.
#'
#' @references
#' Valliant, R. (2020). Comparing alternatives for estimation from
#' nonprobability samples. \emph{Journal of Survey Statistics and Methodology}
#' \bold{8}(2), 231--263. \doi{10.1093/jssam/smz003}
#'
#' Elliott, M.R. and Valliant, R. (2017). Inference for nonprobability
#' samples. \emph{Statistical Science} \bold{32}(2), 249--264.
#'
#' Chrostowski, M.J., Guzman, C.A. and Malm, L. (2025). Variance estimation
#' for non-probability surveys. \emph{Journal of Survey Statistics and
#' Methodology} (forthcoming).
#'
#' Brick, J.M. (2015). Compositional model inference. In
#' \emph{Proceedings of the Section on Survey Research Methods}, pp. 299--307.
#' American Statistical Association, Alexandria, VA.
#'
#' Valliant, R., Dever, J.A. and Kreuter, F. (2018).
#' \emph{Practical Tools for Designing and Weighting Survey Samples}, 2nd ed.
#' Springer, New York.
#'
#' Kolenikov, S. (2014). Calibrating variance estimation with proxy variables.
#' \emph{Survey Methodology} \bold{40}(1), 21--38.
#'
#' Wu, C. (2022). Statistical inference with non-probability survey samples.
#' \emph{Survey Methodology} \bold{48}(2), 283--311.
#'
#' Chen, Y., Li, P. and Wu, C. (2021). Doubly robust inference with
#' non-probability survey samples. \emph{Journal of the American Statistical
#' Association} \bold{115}(532), 2011--2021.
#'
#' @examples
#' # Minimal: pre-computed calibration weights, SRS-based variance
#' df <- data.frame(
#'   y = rnorm(200),
#'   age = sample(c("18-34", "35-54", "55+"), 200, replace = TRUE),
#'   cal_wt = runif(200, 0.5, 2.5)
#' )
#' d <- as_survey_nonprob(df, weights = cal_wt)
#'
#' # Bootstrap variance: replicate weights with calibration re-applied in each
#' set.seed(1)
#' R <- 50
#' rep_cols <- setNames(
#'   as.data.frame(
#'     matrix(runif(200 * R, 0.5, 2.5), nrow = 200)
#'   ),
#'   paste0("rep_", seq_len(R))
#' )
#' df_rep <- cbind(df, rep_cols)
#' d_boot <- as_survey_nonprob(
#'   df_rep,
#'   weights = cal_wt,
#'   repweights = starts_with("rep_"),
#'   type = "bootstrap"
#' )
#'
#' # Jackknife variance (JK1): delete-one replicate weights
#' d_jk <- as_survey_nonprob(
#'   df_rep,
#'   weights = cal_wt,
#'   repweights = starts_with("rep_"),
#'   type = "JK1"
#' )
#' @seealso
#'   [as_survey()] for probability designs with Taylor variance,
#'   [as_survey_replicate()] for replicate-weight designs
#'
#' @family constructors
#' @export
as_survey_nonprob <- function(
  data,
  weights,
  repweights = NULL,
  type = "bootstrap",
  scale = NULL,
  rscales = NULL,
  mse = TRUE,
  reference_sample = NULL,
  calibration = NULL
) {
  call <- match.call()

  # ── Layer 3: data-level validation ─────────────────────────────────────────
  .validate_data_frame(data)

  # ── Resolve weights (required) ──────────────────────────────────────────────

  weights_quo <- rlang::enquo(weights)

  if (rlang::quo_is_missing(weights_quo)) {
    cli::cli_abort(
      c(
        "x" = "{.arg weights} is required for {.fn as_survey_nonprob}.",
        "i" = paste0(
          "Supply the column name of your calibration weight variable ",
          "(e.g., {.code weights = cal_wt})."
        )
      ),
      class = "surveycore_error_weights_missing"
    )
  }

  weights_var <- .resolve_single_col(
    weights_quo,
    data,
    "weights",
    class_none = "surveycore_error_weights_not_found",
    class_multi = "surveycore_error_weights_multiple"
  )

  # ── Validate weight values ──────────────────────────────────────────────────

  .validate_weights(weights_var, data)

  # ── Extract haven metadata ──────────────────────────────────────────────────

  metadata <- .extract_haven_metadata(data)

  # ── Resolve repweights (optional) ──────────────────────────────────────────

  repweights_quo <- rlang::enquo(repweights)
  repweights_vars <- NULL

  if (!rlang::quo_is_null(repweights_quo)) {
    repweights_cols <- tidyselect::eval_select(repweights_quo, data)

    # Error: 0 columns selected
    if (length(repweights_cols) == 0L) {
      cli::cli_abort(
        c("x" = "{.arg repweights} must select at least one column"),
        class = "surveycore_error_repweights_empty"
      )
    }

    # Error: exactly 1 column selected (replicate variance needs >= 2)
    if (length(repweights_cols) == 1L) {
      cli::cli_abort(
        c(
          "x" = paste0(
            "{.arg repweights} must name at least 2 replicate weight ",
            "columns."
          ),
          "i" = "Replicate variance requires >= 2 replicates. Got {.val 1}."
        ),
        class = "surveycore_error_repweights_single"
      )
    }

    repweights_vars <- names(repweights_cols)
    R <- length(repweights_vars)

    # Step 8: normalize "jackknife" alias to "JK1"
    if (
      is.character(type) &&
        length(type) == 1L &&
        identical(type, "jackknife")
    ) {
      type <- "JK1"
    }

    # Step 9: validate type — must be a single string in the supported set
    valid_types <- c("bootstrap", "JK1", "JK2", "JKn")
    if (
      !is.character(type) ||
        length(type) != 1L ||
        is.na(type) ||
        !type %in% valid_types
    ) {
      cli::cli_abort(
        c(
          "x" = paste0(
            "{.arg type} must be one of {.val bootstrap}, {.val JK1},",
            " {.val JK2}, {.val JKn}, or {.val jackknife} for",
            " {.cls survey_nonprob} objects."
          ),
          "i" = "Got {.val {type}}."
        ),
        class = "surveycore_error_type_unsupported_for_nonprob"
      )
    }

    # Step 10: rscales guard for stratified JK (JK2/JKn require explicit rscales)
    if (.is_stratified_jk(type) && is.null(rscales)) {
      cli::cli_abort(
        c(
          "x" = paste0(
            "{.arg type} = {.val {type}} requires explicit {.arg rscales}."
          ),
          "i" = paste0(
            "Stratified jackknife rscales are stratum-specific: ",
            "{.code (n_h - 1) / n_h}. Supplying {.code NULL} would silently ",
            "use {.code rep(1, R)}, which is statistically incorrect for ",
            "JK2/JKn."
          ),
          "v" = paste0(
            "Compute {.code rscales} as {.code (n_h - 1) / n_h} where ",
            "{.code n_h} is the number of units in stratum {.code h}, ",
            "indexed to replicate order."
          )
        ),
        class = "surveycore_error_stratified_jk_rscales_unset"
      )
    }

    # Step 11: scale computation
    if (is.null(scale)) {
      scale <- .compute_nonprob_scale(type, R)
    } else if (scale < 0) {
      cli::cli_abort(
        c(
          "x" = "{.arg scale} must be >= 0. Got {.val {scale}}.",
          "i" = paste0(
            "A negative scale factor produces negative variance, ",
            "which is nonsensical."
          ),
          "v" = paste0(
            "Use {.code scale = 0} to exclude a replicate's contribution, ",
            "or omit {.arg scale} to use the type-specific default."
          )
        ),
        class = "surveycore_error_scale_negative"
      )
    }

    # Step 12: rscales default
    if (is.null(rscales)) {
      rscales <- rep(1, R)
    }

    # Validate rscales (length mismatch and NA/negative)
    .validate_rscales(rscales, R)

    # Validate reference_sample type if provided
    if (
      !is.null(reference_sample) &&
        !S7::S7_inherits(reference_sample, survey_taylor)
    ) {
      cli::cli_abort(
        c(
          "x" = paste0(
            "{.arg reference_sample} must be a {.cls survey_taylor} object."
          ),
          "i" = "Got {.cls {class(reference_sample)[[1L]]}}.",
          "v" = paste0(
            "Pass the {.cls survey_taylor} object used to estimate ",
            "propensity scores."
          )
        ),
        class = "surveycore_error_reference_sample_nonprob"
      )
    }

    # Provenance checks (only when BOTH calibration AND repweights non-NULL)
    if (!is.null(calibration)) {
      # bootstrap provenance check is type-gated: only fires for bootstrap type
      if (type == "bootstrap") {
        if (!isTRUE(calibration$bootstrap)) {
          cli::cli_abort(
            c(
              "x" = paste0(
                "{.arg calibration} indicates the replicate weights were not ",
                "produced by re-running the adjustment procedure."
              ),
              "i" = paste0(
                "{.code calibration$bootstrap} must be {.val TRUE} for ",
                "bootstrap variance to be valid."
              ),
              "v" = paste0(
                "Use {.fn surveywts::create_bootstrap_weights} to produce ",
                "repweights with re-calibration."
              )
            ),
            class = "surveycore_error_provenance_not_bootstrap"
          )
        }
      }
      # R mismatch check applies to ALL types (bootstrap and jackknife)
      if (!is.null(calibration$R) && !identical(calibration$R, R)) {
        cli::cli_abort(
          c(
            "x" = paste0(
              "Provenance records {.val {calibration$R}} replicates but ",
              "{.val {R}} replicate weight columns were found."
            ),
            "i" = paste0(
              "The {.arg calibration} object and {.arg repweights} columns ",
              "must come from the same ",
              "{.fn surveywts::create_bootstrap_weights} call."
            )
          ),
          class = "surveycore_error_provenance_R_mismatch"
        )
      }
    }

    # Build @variables with all 5 repweight keys populated
    variables <- list(
      weights = weights_var,
      repweights = repweights_vars,
      type = type,
      scale = scale,
      rscales = rscales,
      mse = isTRUE(mse),
      probs_provided = FALSE,
      ids = NULL,
      strata = NULL,
      fpc = NULL,
      nest = FALSE,
      visible_vars = NULL
    )
  } else {
    # No repweights — validate reference_sample type if provided
    if (
      !is.null(reference_sample) &&
        !S7::S7_inherits(reference_sample, survey_taylor)
    ) {
      cli::cli_abort(
        c(
          "x" = paste0(
            "{.arg reference_sample} must be a {.cls survey_taylor} object."
          ),
          "i" = "Got {.cls {class(reference_sample)[[1L]]}}.",
          "v" = paste0(
            "Pass the {.cls survey_taylor} object used to estimate ",
            "propensity scores."
          )
        ),
        class = "surveycore_error_reference_sample_nonprob"
      )
    }

    # No-repweights path — all 5 keys present as NULL
    variables <- list(
      weights = weights_var,
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
  }

  # ── Construct and return survey_nonprob object ───────────────────────────

  survey_nonprob(
    data = data,
    metadata = metadata,
    variables = variables,
    calibration = calibration,
    reference_sample = reference_sample,
    call = call
  )
}


# ── as_survey_collection() ────────────────────────────────────────────────────

#' Create a Collection of Survey Designs
#'
#' Builds a [survey_collection] from one or more survey design objects for
#' comparative analysis across waves, cross-sections, or sub-populations.
#' Each element is stored independently — designs are never combined, and
#' variance estimation is never re-specified.
#'
#' @details
#' Arguments may be passed with explicit names (`"wave1" = d1`) or as bare
#' symbols (`d1`, auto-named to `"d1"`). An unnamed argument that is not a
#' bare symbol (e.g., an inline `as_survey(...)` call) raises
#' `surveycore_error_collection_unnamed_expr` — name such arguments
#' explicitly.
#'
#' Duplicate names are repaired by appending `_1`, `_2`, … to subsequent
#' occurrences (first occurrence preserved). When any rename occurs,
#' a `surveycore_warning_collection_duplicate_name_repaired` warning is
#' emitted showing the `original -> repaired` mapping.
#'
#' @param ... One or more `survey_base` objects, passed with explicit names
#'   or as bare symbols. At least one argument is required.
#' @param group <[`tidy-select`][tidyselect::language]> Grouping variable(s)
#'   to apply uniformly across every member survey. Accepts bare names
#'   (`region`, `c(region, stratum)`), `all_of()`, etc. When supplied and
#'   resolving to a non-empty character vector, the named columns must exist
#'   in every member's `@data`; they are propagated onto each member's
#'   `@groups` and set as `coll@groups`. If a member already carries a
#'   non-empty `@groups` that differs from the resolved target, the target
#'   takes precedence and a
#'   `surveycore_warning_collection_group_overridden` warning is emitted
#'   (one per divergent member). When missing or resolving to an empty
#'   vector (`NULL`, `character(0)`, `c()`, `all_of(character(0))`), the
#'   collection adopts the members' uniform `@groups` if they are all
#'   identical, or errors `surveycore_error_collection_group_divergent`
#'   if they differ. Default: missing (adopt-from-members).
#' @param .id Character(1). Identifier column name used when dispatching
#'   analysis functions across the collection. Default `".survey"`. Stored
#'   on the returned collection's `@id` property and used as the default
#'   by `.dispatch_over_collection()` when a per-call `.id` is not
#'   supplied (i.e., when an analysis function is called with `.id = NULL`).
#'   Mutate via [set_collection_id()].
#' @param .if_missing_var Character(1), one of `c("error", "skip")`.
#'   Default `"error"`. Stored on the returned collection's
#'   `@if_missing_var` property and used as the default by
#'   `.dispatch_over_collection()` when a per-call `.if_missing_var` is
#'   not supplied (i.e., when an analysis function is called with
#'   `.if_missing_var = NULL`). When `"skip"`, member surveys missing
#'   a requested variable are dropped from the dispatched result;
#'   when `"error"`, the dispatcher aborts. Mutate via
#'   [set_collection_if_missing_var()].
#'
#' @return A `survey_collection` object containing the supplied surveys.
#'
#' @examples
#' d1 <- as_survey(
#'   gss_2024,
#'   ids = vpsu,
#'   weights = wtssps,
#'   strata = vstrat,
#'   nest = TRUE
#' )
#' d2 <- as_survey(
#'   gss_2024,
#'   ids = vpsu,
#'   weights = wtssps,
#'   strata = vstrat,
#'   nest = TRUE
#' )
#'
#' # Explicit names
#' coll <- as_survey_collection("2020" = d1, "2024" = d2)
#' names(coll)
#'
#' # Bare-symbol auto-naming
#' coll2 <- as_survey_collection(d1, d2)
#' names(coll2)
#'
#' # Uniform grouping across members
#' coll3 <- as_survey_collection(d1, d2, group = vstrat)
#' names(survey_data(coll3[[1L]]))
#' @seealso [survey_collection], [add_survey()], [remove_survey()]
#' @family collections
#' @export
as_survey_collection <- function(
  ...,
  group,
  .id = ".survey",
  .if_missing_var = "error"
) {
  # Validate the new arguments before resolving anything else, so user-
  # facing errors fire early with the same error classes the S7 validator
  # would raise. Both helpers are pure and cheap; the S7 validator runs
  # them again defensively after construction.
  .validate_collection_id(.id, ".id")
  .validate_collection_if_missing_var(.if_missing_var, ".if_missing_var")

  quosures <- rlang::enquos(...)

  if (length(quosures) == 0L) {
    cli::cli_abort(
      c(
        "x" = "At least one survey must be supplied.",
        "v" = paste0(
          "Call with one or more {.cls survey_base} objects, e.g. ",
          "{.code as_survey_collection(wave1 = d1, wave2 = d2)}."
        )
      ),
      class = "surveycore_error_collection_empty"
    )
  }

  caller_names <- .resolve_caller_names(quosures)
  repair <- .repair_collection_names(caller_names)

  if (length(repair$mapping) > 0L) {
    .warn_duplicate_name_repair(repair$mapping)
  }

  surveys <- lapply(quosures, rlang::eval_tidy)
  names(surveys) <- repair$repaired

  # Resolve target @groups vector (Decision 4 / spec §IV.behavior.2).
  group_quo <- rlang::enquo(group)
  group_supplied <- !rlang::quo_is_missing(group_quo)

  if (group_supplied) {
    # Resolve tidy-select against the FIRST member's @data, then verify
    # every other member has those columns.
    first_member <- surveys[[1L]]
    target <- tryCatch(
      tidyselect::eval_select(group_quo, data = first_member@data),
      error = function(e) {
        cli::cli_abort(
          c(
            "x" = paste0(
              "Column (from {.arg group}) not found in member ",
              "{.val {names(surveys)[[1L]]}}."
            ),
            "i" = "Members: {.val {names(surveys)}}.",
            "i" = "Original error: {conditionMessage(e)}"
          ),
          class = "surveycore_error_collection_group_var_not_found",
          parent = e
        )
      }
    )
    target <- names(target)
    # Empty resolution collapses to adopt-from-members branch.
    if (length(target) == 0L) {
      group_supplied <- FALSE
    }
  }

  if (group_supplied) {
    # Validate every OTHER member has all named columns in its @data.
    for (idx in seq_along(surveys)[-1L]) {
      member <- surveys[[idx]]
      member_name <- names(surveys)[[idx]]
      missing_cols <- setdiff(target, names(member@data))
      if (length(missing_cols) > 0L) {
        cli::cli_abort(
          c(
            "x" = paste0(
              "Column {.field {missing_cols[[1L]]}} (from {.arg group}) ",
              "not found in member {.val {member_name}}."
            ),
            "i" = "Members: {.val {names(surveys)}}."
          ),
          class = "surveycore_error_collection_group_var_not_found"
        )
      }
    }

    # Three sub-cases per member (Decision 4).
    for (idx in seq_along(surveys)) {
      member <- surveys[[idx]]
      member_name <- names(surveys)[[idx]]
      member_groups <- member@groups
      if (length(member_groups) == 0L || identical(member_groups, target)) {
        # Silent propagate (empty) or no-op (identical).
        surveys[[idx]]@groups <- target
      } else {
        # Non-empty divergent: propagate + warn (one per member).
        cli::cli_warn(
          c(
            "!" = paste0(
              "Member {.val {member_name}} had @groups ",
              "{.val {member_groups}}; overriding with {.val {target}}."
            ),
            "i" = paste0(
              "The {.arg group} argument to {.fn as_survey_collection} ",
              "takes precedence over pre-existing member grouping."
            ),
            "i" = paste0(
              "If this was unintentional, call {.fn surveytidy::ungroup} ",
              "on the member first, or omit {.arg group} to adopt from ",
              "members."
            )
          ),
          class = "surveycore_warning_collection_group_overridden"
        )
        surveys[[idx]]@groups <- target
      }
    }
  } else {
    # Adopt-from-members branch. Scan all member @groups. If identical
    # (including all-empty), adopt. If divergent, error G2.
    first_groups <- surveys[[1L]]@groups
    divergent <- FALSE
    for (idx in seq_along(surveys)[-1L]) {
      if (!identical(surveys[[idx]]@groups, first_groups)) {
        divergent <- TRUE
        break
      }
    }
    if (divergent) {
      # Summarize divergence for the message.
      member_groups_list <- lapply(surveys, function(s) s@groups)
      summary_strs <- vapply(
        seq_along(surveys),
        function(idx) {
          g <- member_groups_list[[idx]]
          if (length(g) == 0L) {
            paste0(names(surveys)[[idx]], ": <empty>")
          } else {
            paste0(names(surveys)[[idx]], ": ", paste(g, collapse = ", "))
          }
        },
        character(1L)
      )
      cli::cli_abort(
        c(
          "x" = paste0(
            "Member surveys have different @groups, and no {.arg group} ",
            "was supplied."
          ),
          "i" = "Found: {.val {summary_strs}}.",
          "v" = paste0(
            "Supply {.arg group} explicitly, or ungroup members first via ",
            "{.fn surveytidy::ungroup}."
          )
        ),
        class = "surveycore_error_collection_group_divergent"
      )
    }
    target <- first_groups
  }

  survey_collection(
    surveys = surveys,
    groups = target,
    id = .id,
    if_missing_var = .if_missing_var
  )
}
