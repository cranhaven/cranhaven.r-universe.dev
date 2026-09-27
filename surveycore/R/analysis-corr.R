# R/analysis-corr.R
#
# Survey-weighted correlation for survey designs.
# Exported function: get_corr()
#
# Internal helpers: analysis-corr-helpers.R (Pearson variance),
# analysis-corr-latent.R (polychoric / polyserial MLE + variance paths).

# ── .corr_extract_n_failed_from_condition() ──────────────────────────────────
#
# Parse the integer count of non-converged replicates from a
# surveycore_warning_polychoric_replicate_convergence condition message.
# The PC-12 message is formatted as "N of R replicate fit(s) failed to
# converge." with N = n_failed rendered as an unquoted integer by cli.
# Returns NA_integer_ if the count cannot be recovered.
.corr_extract_n_failed_from_condition <- function(cnd) {
  msg <- paste(conditionMessage(cnd), collapse = " ")
  # Match the leading "N of R" in the PC-12 message.
  m <- regmatches(msg, regexpr("[0-9]+(?=\\s+of\\s+[0-9]+)", msg, perl = TRUE))
  if (length(m) != 1L || !nzchar(m)) {
    return(NA_integer_) # nocov
  }
  suppressWarnings(as.integer(m))
}


# ── get_corr() ────────────────────────────────────────────────────────────────

#' Survey-Weighted Correlation (Pearson, Polychoric, Polyserial)
#'
#' Compute pairwise correlations between two or more variables in a survey
#' design, with design-based standard errors and confidence intervals. Returns
#' results in long or wide format. The estimator is selected by `method`:
#' `"pearson"` (default) for two numeric variables, `"polychoric"` for two
#' ordinal variables under a bivariate-normal latent model (Olsson 1979),
#' or `"polyserial"` for one ordinal + one continuous variable (Cox 1974).
#' The survey-weighted polychoric and polyserial estimators (point estimates
#' and design-based variance) are implemented from scratch following
#' Mannan (2025); they are not derived from the `survey` package, which does
#' not provide these estimators.
#'
#' @param design A survey design object: `survey_taylor`, `survey_replicate`,
#'   `survey_twophase`, or `survey_nonprob`. `method` values `"polychoric"`
#'   and `"polyserial"` are supported on `survey_taylor`, `survey_replicate`,
#'   and `survey_nonprob` designs that supply replicate weights
#'   (`repweights` argument in `as_survey_nonprob()`). `survey_nonprob`
#'   without replicate weights and `survey_twophase` designs raise
#'   `surveycore_error_polychoric_design_unsupported` for latent methods.
#' @param x <[`tidy-select`][tidyselect::language]> Two or more unquoted
#'   variable names. For `method = "pearson"`, non-numeric columns are dropped
#'   with a warning. For `method = "polychoric"`, every selected column must
#'   classify as ordinal (ordered factor, unordered factor, or integer with
#'   `<= 10` distinct values) — non-ordinal columns raise
#'   `surveycore_error_polychoric_requires_ordinal`. For
#'   `method = "polyserial"`, each pair is canonicalized by type (one ordinal
#'   + one continuous); logical / character / high-cardinality integer
#'   columns raise `surveycore_error_polyserial_canonicalization_ambiguous`.
#' @param group <[`tidy-select`][tidyselect::language]> Optional grouping
#'   variable(s). Combined with any grouping set by `group_by()`. Default
#'   `NULL`.
#' @param format `"long"` (default) or `"wide"`. Long format returns one row
#'   per variable pair with inference statistics. Wide format returns the
#'   correlation matrix (`r` values only — no variance or inference columns).
#'   When `group` is active, group columns are prepended in both formats.
#'   Case-sensitive.
#' @param redundant Logical. If `FALSE` (default), each pair appears once
#'   (lower triangle: pairs where var1 precedes var2 in input order). If
#'   `TRUE`, both `(A, B)` and `(B, A)` are included (full directed pairs).
#'   Only affects long format; wide format always shows the full symmetric
#'   matrix.
#' @param diagonal Logical. If `FALSE` (default), self-correlations are
#'   excluded (diagonal is `NA` in wide format). If `TRUE`, self-correlations
#'   (r equals 1) are included.
#' @param variance `NULL` or a character vector of one or more of `"se"`,
#'   `"ci"`, `"var"`, `"cv"`, `"moe"`, `"deff"`. Default `"ci"`. CI bounds
#'   use the Fisher Z transform (guaranteeing bounds in (-1, 1)). Only
#'   applies to long format.
#' @param conf_level Numeric scalar in (0, 1). Default `0.95`.
#' @param n_weighted Logical. If `TRUE`, add an `n_weighted` column with the
#'   pairwise sum of weights (both variables non-NA). Default `FALSE`.
#' @param decimals Integer or `NULL`. If an integer, rounds all numeric output
#'   columns (e.g., `r`, `se`, `ci_low`, `ci_high`) to this many decimal
#'   places. Default `NULL` (no rounding). Silently ignored when
#'   `format = "wide"` (the wide matrix contains only `r` values, which are
#'   not rounded by this argument).
#' @param min_cell_n Integer. Minimum pairwise unweighted count before
#'   `surveycore_warning_small_cell` fires. Default `30L` (AAPOR guidance).
#' @param na.rm Logical. Controls `NA` handling for group variables and the
#'   computation domain. Pairwise complete-case deletion is always applied
#'   for the correlation variables themselves regardless of this flag. If
#'   `TRUE` (default), observations where any group variable is `NA` are
#'   excluded from the output. If `FALSE`, observations where a group variable
#'   is `NA` are collected into their own group row in the output (appearing
#'   after all non-`NA` group rows).
#' @param label_values Logical. If `TRUE` (default) and the grouping variable
#'   has value labels, the group column is converted to a labelled factor.
#'   Has no visible effect when no groups are active.
#' @param label_vars Logical. If `TRUE` (default) and variable labels are
#'   set in metadata, `var1`/`var2` columns (long) and `variable` column
#'   (wide) show labels instead of raw names. Falls back to raw names if
#'   labels are unset.
#' @param name_style `"surveycore"` (default) or `"broom"`. When `"broom"`,
#'   renames `r` → `estimate`, `se` → `std.error`, etc. Only affects long
#'   format.
#' @param method Character(1). Estimator applied to every pair. One of
#'   `"pearson"` (default, sample-based product-moment correlation),
#'   `"polychoric"` (MLE under a bivariate-normal latent model for two
#'   ordinal variables), or `"polyserial"` (MLE for one ordinal + one
#'   continuous variable). The same `method` applies to every pair; it
#'   cannot be vectorised. Non-matching values raise the standard
#'   [base::match.arg()] signal.
#' @param ... Unused. Reserved so that `.id` and `.if_missing_var` remain
#'   named-only when a `survey_collection` is passed as `design`.
#' @param .id Character(1) or `NULL`. Column name used to identify each
#'   survey when `design` is a [`survey_collection`]. For collection inputs,
#'   `NULL` (the default) resolves to the collection's stored `@id` property.
#'   Pass a non-`NULL` value to override. Ignored when `design` is a single
#'   survey.
#' @param .if_missing_var `"error"`, `"skip"`, or `NULL`. How to handle
#'   surveys in a collection that lack one of the requested NSE variables.
#'   For collection inputs, `NULL` (the default) resolves to the collection's
#'   stored `@if_missing_var` property. Pass a non-`NULL` value to override.
#'   Ignored when `design` is a single survey.
#'
#' @return A `survey_corr` tibble (also inheriting `survey_result`).
#'
#'   When `group` is active, group variable columns are prepended before all
#'   other columns in both long and wide formats.
#'
#'   **Long format** columns:
#'   \itemize{
#'     \item `[group_cols...]` — group variable columns (when active), first.
#'     \item `var1`, `var2` — variable names (or labels when
#'       `label_vars = TRUE`).
#'     \item `r` — correlation coefficient. For `method = "pearson"`, the
#'       weighted product-moment correlation; for `"polychoric"` /
#'       `"polyserial"`, the MLE of `rho` under a bivariate-normal latent
#'       model.
#'     \item Variance columns (`se`, `var`, `cv`, `ci_low`, `ci_high`, `moe`,
#'       `deff`) — only those requested via `variance`.
#'     \item `p_value` — two-tailed p-value.
#'     \item `statistic` — test statistic. A t-statistic for
#'       `method = "pearson"`; a Wald z-statistic for latent methods.
#'     \item `df` — degrees of freedom. `n - 2` for `method = "pearson"`;
#'       `NA_integer_` for `"polychoric"` and `"polyserial"` (asymptotic
#'       normal distribution is used).
#'     \item `n` — pairwise unweighted count.
#'     \item `n_weighted` — pairwise sum of weights (only when requested).
#'   }
#'
#'   **Wide format** columns:
#'   \itemize{
#'     \item `[group_cols...]` — group variable columns (when active), first.
#'     \item `variable` — row variable names (or labels).
#'     \item One column per focal variable, containing `r` values.
#'   }
#'
#'   Use `meta(result)` to access design type, variable labels, and
#'   `method` (`"pearson"`, `"polychoric"`, or `"polyserial"`). For
#'   `method != "pearson"`, `meta(result)$bivariate_normal_cdf` is
#'   `"pbivnorm"` (the bivariate-normal CDF used internally). When the
#'   replicate variance path observed one or more non-converged replicates,
#'   `meta(result)$n_failed_replicates_total` carries the scalar total.
#'
#' @details
#' **Polychoric / polyserial semantics.** For `method != "pearson"`, each pair
#' is fit by a two-step MLE: weighted marginal thresholds (and, for
#' polyserial, a weighted standardization of the continuous side) are
#' estimated first, then `rho` is maximised over the weighted
#' log-likelihood via [stats::optimize()] on `(-1 + 1e-6, 1 - 1e-6)`.
#' Confidence intervals are constructed on the Fisher-z scale
#' (`atanh(rho)`) and back-transformed via `tanh` with truncation to
#' `[-1, 1]`. The Wald statistic `zeta.hat / SE(zeta.hat)` is referred to
#' a standard normal distribution, so `df = NA_integer_` — distinct from
#' the Pearson case where `df = n - 2` and the t-distribution is used.
#' Column label attributes are method-neutral (e.g. `"statistic"`, not
#' `"t-statistic"` / `"z-statistic"`); check `meta(result)$method` to
#' interpret the values.
#'
#' **Bivariate-normal assumption.** The polychoric / polyserial MLEs
#' assume the underlying latent variables are jointly bivariate-normal.
#' This is an unverified assumption; no runtime diagnostic is performed.
#'
#' **Taylor-path cost.** On a `survey_taylor` design, the variance path
#' for `method != "pearson"` is `O(n)` re-optimisations per variable pair
#' (a perturbation-based influence function). For large `n` and many
#' pairs, passing a `survey_replicate` design (one re-fit per replicate,
#' not per respondent) is substantially faster.
#'
#' **Replicate-type caveat.** Mannan (2025) verifies the replicate-weight
#' variance formula for jackknife and bootstrap replicates. BRR and Fay
#' replicates are admitted mechanically via the design's stored `scale`
#' / `rscales` coefficients, but the paper does not validate their
#' behaviour for this non-linear pseudo-likelihood estimator.
#'
#' @references
#' Cox, N. R. (1974). Estimation of the correlation between a continuous
#'   and a discrete variable. *Biometrics*, 30(1), 171-178.
#'
#' Mannan, H. (2025). SAS programs for estimation of weighted polychoric
#'   and weighted polyserial correlations in a complex survey. SSRN.
#'   \doi{10.2139/ssrn.6580480}
#'
#' Olsson, U. (1979). Maximum likelihood estimation of the polychoric
#'   correlation coefficient. *Psychometrika*, 44(4), 443-460.
#'
#' @examples
#' d <- as_survey(
#'   nhanes_2017,
#'   ids = sdmvpsu,
#'   weights = wtint2yr,
#'   strata = sdmvstra,
#'   nest = TRUE
#' )
#' get_corr(d, x = c(ridageyr, bpxsy1))
#'
#' # Wide correlation matrix
#' get_corr(d, x = c(ridageyr, bpxsy1), format = "wide")
#'
#' # AAPOR-compliant
#' get_corr(
#'   d,
#'   x = c(ridageyr, bpxsy1),
#'   variance = c("ci", "moe"),
#'   n_weighted = TRUE
#' )
#'
#' # Polychoric correlation between two ordinal variables
#' df <- data.frame(
#'   id = 1:200,
#'   wt = runif(200, 0.5, 2),
#'   o1 = factor(sample(1:4, 200, replace = TRUE), ordered = TRUE),
#'   o2 = factor(sample(1:4, 200, replace = TRUE), ordered = TRUE)
#' )
#' d_ord <- as_survey(df, weights = wt)
#' get_corr(d_ord, x = c(o1, o2), method = "polychoric")
#' @family analysis
#' @export
get_corr <- function(
  design,
  x,
  group = NULL,
  format = c("long", "wide"),
  redundant = FALSE,
  diagonal = FALSE,
  variance = "ci",
  conf_level = 0.95,
  n_weighted = FALSE,
  decimals = NULL,
  min_cell_n = 30L,
  na.rm = TRUE,
  label_values = TRUE,
  label_vars = TRUE,
  name_style = "surveycore",
  method = "pearson",
  ...,
  .id = NULL,
  .if_missing_var = NULL
) {
  if (S7::S7_inherits(design, survey_collection)) {
    return(.dispatch_over_collection(
      get_corr,
      design,
      x = {{ x }},
      group = {{ group }},
      method = method,
      ...,
      .id = .id,
      .if_missing_var = .if_missing_var
    ))
  }
  # ── Step 1: Validate ────────────────────────────────────────────────────────
  .check_unsupported_class(design, "get_corr")
  .validate_shared_args(
    variance,
    conf_level,
    name_style,
    decimals = decimals,
    na.rm = na.rm
  )
  format <- match.arg(format)
  method <- match.arg(method, c("pearson", "polychoric", "polyserial"))

  # ── Step 2: Resolve variables ───────────────────────────────────────────────
  x_quo <- rlang::enquo(x)
  group_quo <- rlang::enquo(group)

  x_names_all <- .resolve_tidy_select(x_quo, design@data)

  # Method-aware non-numeric handling. Pearson: silently drop non-numeric
  # columns with a warning. Polychoric / polyserial: retain all columns and
  # let the latent-pair dispatcher gate on ordinal / canonicalization errors.
  if (identical(method, "pearson")) {
    is_numeric_col <- vapply(
      x_names_all,
      function(nm) is.numeric(design@data[[nm]]),
      logical(1L)
    )
    dropped <- x_names_all[!is_numeric_col]
    if (length(dropped) > 0L) {
      cli::cli_warn(
        c(
          "!" = paste0(
            "{.fn get_corr} requires numeric variables. ",
            "Dropping non-numeric column{?s}: {.field {dropped}}."
          )
        ),
        class = "surveycore_warning_corr_non_numeric"
      )
    }
    x_names <- x_names_all[is_numeric_col]
  } else {
    x_names <- x_names_all
  }

  if (length(x_names) < 2L) {
    cli::cli_abort(
      c(
        "x" = paste0(
          "{.fn get_corr} requires at least 2 variables, but {.arg x} ",
          "resolved to {length(x_names)} variable{?s}."
        )
      ),
      class = "surveycore_error_insufficient_variables"
    )
  }

  # ── Step 3: Domain mask ─────────────────────────────────────────────────────
  domain_mask <- .apply_domain(design)

  # ── Step 4: Collect variable metadata ───────────────────────────────────────
  x_meta_list <- lapply(x_names, function(nm) .extract_var_meta(design, nm))
  names(x_meta_list) <- x_names

  # ── Step 5: Determine display names (labels or raw names) ───────────────────
  display_names <- if (isTRUE(label_vars)) {
    vapply(
      x_names,
      function(nm) {
        lbl <- x_meta_list[[nm]]$variable_label
        if (!is.null(lbl) && nchar(as.character(lbl)) > 0L) {
          as.character(lbl)
        } else {
          nm
        }
      },
      character(1L)
    )
  } else {
    x_names
  }
  names(display_names) <- x_names

  # ── Step 6: Build pair list ─────────────────────────────────────────────────
  p <- length(x_names)
  pairs_i <- integer(0)
  pairs_j <- integer(0)
  for (i in seq_len(p - 1L)) {
    for (j in seq(i + 1L, p)) {
      pairs_i <- c(pairs_i, i)
      pairs_j <- c(pairs_j, j)
    }
  }
  n_pairs <- length(pairs_i)

  # ── Step 7: Resolve group vars ──────────────────────────────────────────────
  group_vars <- .resolve_groups(design, group_quo)

  # ── Step 8: Single-level warning per group var ──────────────────────────────
  if (length(group_vars) > 0L) {
    for (gv in group_vars) {
      gv_vals <- design@data[[gv]][domain_mask]
      uniq_lvls <- unique(gv_vals[!is.na(gv_vals)])
      if (length(uniq_lvls) < 2L) {
        cli::cli_warn(
          c(
            "!" = paste0(
              "Grouping variable {.field {gv}} has only one observed level ",
              if (length(uniq_lvls) == 1L) {
                "({.val {as.character(uniq_lvls[[1L]])}})."
              } else {
                "."
              },
              " Grouped estimates will have a single row."
            )
          ),
          class = "surveycore_warning_single_level"
        )
      }
    }
  }

  # ── Step 9: Build group combinations ────────────────────────────────────────
  if (length(group_vars) > 0L) {
    domain_data <- design@data[domain_mask, group_vars, drop = FALSE]
    group_combos <- .build_group_combos(domain_data, na.rm)
    n_combos <- nrow(group_combos)
  } else {
    group_combos <- data.frame()
    n_combos <- 1L
  }

  # ── Step 10: Outer group / inner pair loops ─────────────────────────────────
  all_pair_results <- vector("list", n_combos)
  all_grp_rows <- vector("list", n_combos)
  all_active_masks <- vector("list", n_combos)
  small_cell_ns <- integer(0)
  n_failed_replicates_total <- 0L

  for (ci in seq_len(n_combos)) {
    # Build active mask for this combo
    if (length(group_vars) > 0L) {
      combo_row <- group_combos[ci, , drop = FALSE]
      data_cols <- as.list(design@data[group_vars])
      group_match <- .match_group_combo(data_cols, combo_row)
      active_mask <- domain_mask & group_match
      all_grp_rows[[ci]] <- combo_row
    } else {
      active_mask <- domain_mask
    }
    all_active_masks[[ci]] <- active_mask
    active_domain <- as.numeric(active_mask)

    # Inner loop: pairs. Dispatch by method: Pearson uses the existing
    # HT / Hajek delta-method path; polychoric / polyserial delegate to
    # .corr_latent_pair() (MLE + design-based Fisher-z variance).
    pair_results_this <- vector("list", n_pairs)
    for (k in seq_len(n_pairs)) {
      xnm <- x_names[[pairs_i[[k]]]]
      ynm <- x_names[[pairs_j[[k]]]]
      if (identical(method, "pearson")) {
        vco <- .corr_vcov_pair(design, xnm, ynm, active_domain, na.rm)
        res <- .corr_pair_result(vco)
      } else {
        # Capture PC-12 (replicate partial convergence) warnings so we can
        # aggregate the total number of non-converged replicates across
        # pairs into meta()$n_failed_replicates_total. The handler does NOT
        # muffle the warning; it observes it and passes through.
        res <- withCallingHandlers(
          .corr_latent_pair(
            design = design,
            x_col = xnm,
            y_col = ynm,
            method = method,
            active_domain = active_domain,
            na.rm = na.rm,
            conf_level = conf_level
          ),
          surveycore_warning_polychoric_replicate_convergence = function(cnd) {
            add <- .corr_extract_n_failed_from_condition(cnd)
            if (!is.na(add)) {
              n_failed_replicates_total <<- n_failed_replicates_total +
                as.integer(add)
            }
          }
        )
      }
      pair_results_this[[k]] <- res
      if (!is.na(res$n) && res$n > 0L && res$n < min_cell_n) {
        small_cell_ns <- c(small_cell_ns, res$n)
      }
    }
    all_pair_results[[ci]] <- pair_results_this
  }

  # ── Step 11: Small-cell warning (fires once, after all groups) ──────────────
  n_small <- length(small_cell_ns)
  if (n_small > 0L) {
    cli::cli_warn(
      c(
        "!" = paste0(
          "{n_small} cell{?s} {?has/have} fewer than {min_cell_n} unweighted ",
          "observations. Estimates in these cells may be unreliable for ",
          "public reporting (AAPOR guidance)."
        )
      ),
      class = "surveycore_warning_small_cell"
    )
  }

  # ── Step 12: Group metadata ─────────────────────────────────────────────────
  group_meta <- .build_group_meta(design, group_vars)

  # ── Step 13: Wide format path (grouped) ─────────────────────────────────────
  if (format == "wide") {
    wide_col_vecs_list <- vector("list", n_combos)
    wide_grp_rows <- vector("list", n_combos)

    for (ci in seq_len(n_combos)) {
      wide_col_vecs_list[[ci]] <- .corr_build_matrix_col_vecs(
        x_names,
        display_names,
        pairs_i,
        pairs_j,
        all_pair_results[[ci]],
        diagonal
      )
      if (length(group_vars) > 0L) {
        wide_grp_rows[[ci]] <- all_grp_rows[[ci]][rep(1L, p), , drop = FALSE]
      }
    }

    # Stack col_vecs across combos
    all_col_names <- names(wide_col_vecs_list[[1L]])
    stacked_wide <- lapply(all_col_names, function(nm) {
      unlist(lapply(wide_col_vecs_list, `[[`, nm), use.names = FALSE)
    })
    names(stacked_wide) <- all_col_names

    # Build groups_df for wide result
    if (length(group_vars) > 0L) {
      wide_groups_df <- do.call(rbind, wide_grp_rows)
      rownames(wide_groups_df) <- NULL
      wide_groups_df <- .apply_group_labels(
        wide_groups_df,
        group_vars,
        design,
        label_values
      )
    } else {
      wide_groups_df <- data.frame()
    }

    meta_args_wide <- list(
      conf_level = conf_level,
      call = match.call(),
      method = method,
      group = group_meta,
      x = x_meta_list
    )
    if (!identical(method, "pearson")) {
      meta_args_wide$bivariate_normal_cdf <- "pbivnorm"
    }
    if (!identical(method, "pearson") && n_failed_replicates_total > 0L) {
      meta_args_wide$n_failed_replicates_total <- as.integer(
        n_failed_replicates_total
      )
    }

    return(.make_result_tibble(
      stacked_wide,
      wide_groups_df,
      "survey_corr",
      design,
      meta_args_wide,
      CORR_META_KEYS
    ))
  }

  # ── Step 14: Long format — pre-loop constants ───────────────────────────────
  # Expand pairs according to redundant/diagonal flags (same for every group)
  row_i <- integer(0)
  row_j <- integer(0)
  for (k in seq_len(n_pairs)) {
    row_i <- c(row_i, pairs_i[[k]])
    row_j <- c(row_j, pairs_j[[k]])
    if (isTRUE(redundant)) {
      row_i <- c(row_i, pairs_j[[k]])
      row_j <- c(row_j, pairs_i[[k]])
    }
  }
  if (isTRUE(diagonal)) {
    row_i <- c(row_i, seq_len(p))
    row_j <- c(row_j, seq_len(p))
  }
  n_rows_per_combo <- length(row_i)

  # Lookup from (i, j) pair index to pair_results index
  pair_idx_map <- matrix(NA_integer_, p, p)
  for (k in seq_len(n_pairs)) {
    pair_idx_map[pairs_i[[k]], pairs_j[[k]]] <- k
    pair_idx_map[pairs_j[[k]], pairs_i[[k]]] <- k
  }

  # ── Step 15: Pre-allocate accumulator vectors ───────────────────────────────
  total_rows <- n_combos * n_rows_per_combo
  acc_var1 <- character(total_rows)
  acc_var2 <- character(total_rows)
  acc_r <- numeric(total_rows)
  acc_se <- numeric(total_rows)
  acc_se_srs <- numeric(total_rows)
  acc_pval <- numeric(total_rows)
  acc_stat <- numeric(total_rows)
  acc_df <- integer(total_rows)
  acc_n <- integer(total_rows)
  acc_nw <- numeric(total_rows)
  # Non-Pearson path precomputes Fisher-z CI bounds per pair; collect them
  # so the variance-column assembly can consume them directly.
  acc_ci_low <- rep(NA_real_, total_rows)
  acc_ci_high <- rep(NA_real_, total_rows)
  acc_grp_rows <- vector("list", n_combos)

  is_latent <- !identical(method, "pearson")

  # ── Step 16: Combo fill loop ────────────────────────────────────────────────
  for (ci in seq_len(n_combos)) {
    offset <- (ci - 1L) * n_rows_per_combo
    idx <- offset + seq_len(n_rows_per_combo)
    active_mask <- all_active_masks[[ci]]
    pair_results <- all_pair_results[[ci]]

    for (ri in seq_len(n_rows_per_combo)) {
      i <- row_i[[ri]]
      j <- row_j[[ri]]
      out <- offset + ri

      acc_var1[[out]] <- display_names[[x_names[[i]]]]
      acc_var2[[out]] <- display_names[[x_names[[j]]]]

      if (i == j) {
        # Self-correlation (diagonal = TRUE case). Same for Pearson and
        # latent methods: r = 1 exactly, SE = 0; df / p / statistic are
        # method-aware (Wald reference is standard normal for latent).
        acc_r[[out]] <- 1
        acc_se[[out]] <- 0
        acc_se_srs[[out]] <- 0
        acc_pval[[out]] <- 0
        acc_stat[[out]] <- Inf
        acc_df[[out]] <- if (is_latent) NA_integer_ else 0L
        acc_n[[out]] <- as.integer(
          sum(active_mask & !is.na(design@data[[x_names[[i]]]]))
        )
        acc_nw[[out]] <- 0
        if (is_latent) {
          acc_ci_low[[out]] <- 1
          acc_ci_high[[out]] <- 1
        }
      } else {
        k <- pair_idx_map[i, j]
        res <- pair_results[[k]]
        r <- res$r
        se <- res$se_r
        nn <- res$n

        acc_r[[out]] <- r
        acc_se[[out]] <- se
        acc_se_srs[[out]] <- res$se_srs
        acc_n[[out]] <- as.integer(nn)
        acc_nw[[out]] <- res$n_weighted

        if (is_latent) {
          # Polychoric / polyserial: z-scale Wald statistic referred to a
          # standard normal. df = NA_integer_ since the MLE is asymptotic.
          # CI bounds were pre-computed on the Fisher-z scale by
          # .corr_latent_pair() and truncated to [-1, 1].
          acc_ci_low[[out]] <- res$ci_low %||% NA_real_
          acc_ci_high[[out]] <- res$ci_high %||% NA_real_
          rho_z <- res$rho_z %||% NA_real_
          se_z <- res$se_z %||% NA_real_
          if (
            !is.na(rho_z) &&
              !is.na(se_z) &&
              is.finite(se_z) &&
              se_z > 0
          ) {
            z_stat <- rho_z / se_z
            acc_stat[[out]] <- z_stat
            acc_df[[out]] <- NA_integer_
            acc_pval[[out]] <- 2 * stats::pnorm(-abs(z_stat))
          } else {
            acc_stat[[out]] <- NA_real_
            acc_df[[out]] <- NA_integer_
            acc_pval[[out]] <- NA_real_
          }
        } else if (!is.na(r) && !is.na(nn) && nn >= 3L) {
          df_t <- nn - 2L
          if (abs(r) < 1) {
            t_stat <- r * sqrt(df_t) / sqrt(1 - r^2)
          } else {
            t_stat <- if (r > 0) Inf else -Inf
          }
          acc_stat[[out]] <- t_stat
          acc_df[[out]] <- df_t
          acc_pval[[out]] <- 2 * stats::pt(-abs(t_stat), df = df_t)
        } else {
          acc_stat[[out]] <- NA_real_
          acc_df[[out]] <- NA_integer_
          acc_pval[[out]] <- NA_real_
        }
      }
    }

    if (length(group_vars) > 0L) {
      grp_block <- all_grp_rows[[ci]][rep(1L, n_rows_per_combo), , drop = FALSE]
      acc_grp_rows[[ci]] <- grp_block
    }
  }

  # ── Step 17: Build groups_df ────────────────────────────────────────────────
  if (length(group_vars) > 0L) {
    groups_df <- do.call(rbind, acc_grp_rows)
    rownames(groups_df) <- NULL
    groups_df <- .apply_group_labels(
      groups_df,
      group_vars,
      design,
      label_values
    )
  } else {
    groups_df <- data.frame()
  }

  # ── Step 18: Build variance columns ─────────────────────────────────────────
  r_vec <- acc_r
  se_vec <- acc_se
  sesrs_vec <- acc_se_srs

  var_col_list <- list()

  if (!is.null(variance)) {
    if ("se" %in% variance) {
      var_col_list$se <- se_vec
    }
    if ("var" %in% variance) {
      var_col_list$var <- se_vec^2
    }
    if ("cv" %in% variance) {
      cv <- se_vec / r_vec
      is_undef <- !is.na(r_vec) & r_vec <= 0
      n_undef <- sum(is_undef)
      if (n_undef > 0L) {
        cv[is_undef] <- NA_real_
        cli::cli_warn(
          c(
            "!" = paste0(
              '{.arg variance = "cv"} is undefined for {n_undef} ',
              "cell{?s} where the correlation is 0 or negative."
            ),
            "i" = "{.code cv} set to {.code NA} for those cells."
          ),
          class = "surveycore_warning_cv_undefined"
        )
      }
      var_col_list$cv <- cv
    }
    if ("ci" %in% variance || "moe" %in% variance) {
      if (is_latent) {
        # CI bounds were pre-computed per-pair on the Fisher-z scale by
        # .corr_latent_pair() and truncated to [-1, 1]. Reuse them to keep
        # the z-scale / rho-scale transform in one place.
        ci_low <- acc_ci_low
        ci_high <- acc_ci_high
      } else {
        z_crit <- stats::qnorm((1 + conf_level) / 2)
        ci_low <- tanh(atanh(r_vec) - z_crit * se_vec)
        ci_high <- tanh(atanh(r_vec) + z_crit * se_vec)
      }
      if ("ci" %in% variance) {
        var_col_list$ci_low <- ci_low
        var_col_list$ci_high <- ci_high
      }
      if ("moe" %in% variance) {
        var_col_list$moe <- (ci_high - ci_low) / 2
      }
    }
    if ("deff" %in% variance) {
      deff <- (se_vec / sesrs_vec)^2
      deff[is.na(sesrs_vec) | sesrs_vec <= 0] <- NA_real_
      var_col_list$deff <- deff
    }
  }

  # ── Step 19: Assemble col_vecs in canonical order ───────────────────────────
  col_vecs <- list(var1 = acc_var1, var2 = acc_var2, r = r_vec)
  col_vecs <- c(col_vecs, var_col_list)
  col_vecs$p_value <- acc_pval
  col_vecs$statistic <- acc_stat
  col_vecs$df <- acc_df
  col_vecs$n <- acc_n
  if (isTRUE(n_weighted)) {
    col_vecs$n_weighted <- acc_nw
  }

  # ── Step 20: Build meta_args ────────────────────────────────────────────────
  meta_args <- list(
    conf_level = conf_level,
    call = match.call(),
    method = method,
    group = group_meta,
    x = x_meta_list
  )
  if (is_latent) {
    meta_args$bivariate_normal_cdf <- "pbivnorm"
  }
  if (is_latent && n_failed_replicates_total > 0L) {
    meta_args$n_failed_replicates_total <- as.integer(
      n_failed_replicates_total
    )
  }

  # ── Step 21: Assemble result ────────────────────────────────────────────────
  result <- .make_result_tibble(
    col_vecs,
    groups_df,
    "survey_corr",
    design,
    meta_args,
    CORR_META_KEYS
  )

  # ── Step 22: Convert var1/var2 to factors (levels in variable supply order) ─
  # Display strings determine the factor level values; supply order is
  # preserved.
  # Same uniq_display levels across all groups.
  uniq_display <- unique(display_names)
  result$var1 <- factor(result$var1, levels = uniq_display)
  result$var2 <- factor(result$var2, levels = uniq_display)

  # ── Step 23: Attach method-neutral labels for `statistic`, `df`, `p_value` ─
  # The spec requires these three columns carry method-neutral `label`
  # attributes (e.g., "statistic", not "t-statistic" / "z-statistic") so
  # downstream gt consumers render consistently across Pearson and the
  # latent methods. Other columns are left un-labelled in this release to
  # preserve existing tests — full column-label coverage is a separate
  # follow-up (see MEMORY.md project_column_labels).
  result <- .attach_corr_inference_labels(result, name_style)

  # ── Step 24: Apply decimals and name style ──────────────────────────────────
  if (!is.null(decimals)) {
    result <- .apply_decimals(result, decimals)
  }
  .apply_name_style(result, name_style)
}


# ── .attach_corr_inference_labels() ──────────────────────────────────────────
#
# Attach method-neutral `label` attributes to the three inference columns
# (`statistic`, `df`, `p_value`) of a survey_corr result. For name_style =
# "broom", the p_value column is named `p.value`.
.attach_corr_inference_labels <- function(result, name_style) {
  saved_meta <- attr(result, ".meta")
  saved_class <- class(result)

  p_value_col <- if (identical(name_style, "broom")) "p.value" else "p_value"

  if ("statistic" %in% names(result)) {
    attr(result[["statistic"]], "label") <- "statistic"
  }
  if ("df" %in% names(result)) {
    attr(result[["df"]], "label") <- "df"
  }
  if (p_value_col %in% names(result)) {
    attr(result[[p_value_col]], "label") <- "p-value"
  }

  attr(result, ".meta") <- saved_meta
  class(result) <- saved_class
  result
}
