# R/analysis-covariance.R
#
# Design-based finite-population covariance estimation for survey designs.
# Exported function: get_covariance()
#
# Internal helpers: analysis-covariance-helpers.R

# ── get_covariance() ──────────────────────────────────────────────────────────

#' Design-Based Population Covariance for a Survey Design
#'
#' Compute the design-based estimate of the finite-population Pearson
#' covariance for every (unordered, by default) pair of numeric variables
#' selected from `x`, with optional grouping, uncertainty quantification,
#' and metadata-driven labelling. Matches the off-diagonal entries of
#' `survey::svyvar()` (Kish `n/(n-1)` correction) on Taylor, replicate,
#' twophase, and nonprob designs at numerical parity.
#'
#' @details
#' Confidence intervals use the normal-Wald approximation on the SE of the
#' covariance estimate: `ci_low = covariance - z * se`,
#' `ci_high = covariance + z * se`, where `z = qnorm((1 + conf_level) / 2)`.
#' The bounds are **not clamped**. Covariance is unbounded — `ci_low` and
#' `ci_high` may have opposite signs and may cross zero. Users who want
#' clamped intervals can post-process. This behaviour matches
#' `survey::svyvar()`.
#'
#' NA handling is **pairwise-complete per pair**: each ordered pair drops
#' rows where either variable is `NA`. There is no `na_handling` argument;
#' pairwise is the only policy. This matches `survey::svyvar()` off-diagonal
#' pair-at-a-time semantics, **not** `svyvar()`'s default listwise deletion
#' across a multi-variable formula. Numerical parity therefore only holds
#' when oracle calls are made pair-at-a-time
#' (`survey::svyvar(~x + y, design)` per pair).
#'
#' Under `diagonal = TRUE`, the self-pair `(x, x)` returns the design-based
#' Kish-corrected **variance** of `x` on the active domain — not `1` as in
#' `get_corr()`. The covariance matrix diagonal is the variance vector, not
#' the identity. The diagonal-parity gate guarantees that
#' `get_covariance(d, c(x, x), diagonal = TRUE)$covariance` and `$se` equal
#' `get_variance(d, x)$variance` and `$se` numerically (point at `1e-10`,
#' SE at `1e-8`) when the active domains match.
#'
#' Design effect (`deff`) uses the Goodnight / Mood-Graybill SRS reference
#' `SE_SRS(cov) = sqrt((Var(x) * Var(y) + cov^2) / (n - 1))`. When both
#' the design SE and SRS SE are zero (constant-variable pairs), `deff` is
#' set to exactly `0` (0 / 0 guard).
#'
#' @param design A survey design object: `survey_taylor`, `survey_replicate`,
#'   `survey_twophase`, or `survey_nonprob`. Also accepts a
#'   [`survey_collection`].
#' @param x <[`tidy-select`][tidyselect::language]> Two or more unquoted
#'   variable names. Must resolve to at least two columns. Non-numeric
#'   columns are dropped with a warning; if fewer than 2 numeric variables
#'   remain, an error is raised.
#' @param group <[`tidy-select`][tidyselect::language]> Optional grouping
#'   variable(s). Combined with any grouping set by `group_by()`. Default
#'   `NULL`. Covariances are estimated separately within each group using
#'   that group's own weighted means for centring.
#' @param redundant Logical. If `FALSE` (default), each unordered pair
#'   appears once in supply order (lower-triangle). If `TRUE`, both
#'   `(A, B)` and `(B, A)` are emitted.
#' @param diagonal Logical. If `FALSE` (default), self-pairs `(x, x)` are
#'   excluded. If `TRUE`, one self-pair per variable is emitted with
#'   `covariance = \eqn{\widehat{\mathrm{Var}}(x)}{Var_hat(x)}` (the
#'   design-based variance -- **not** `1`).
#' @param variance `NULL` or a character vector of one or more of `"se"`,
#'   `"ci"`, `"var"`, `"cv"`, `"moe"`, `"deff"`. Default `"ci"`.
#' @param conf_level Numeric scalar in `(0, 1)`. Default `0.95`.
#' @param n_weighted Logical. If `TRUE`, append an `n_weighted` column
#'   with the pair's pairwise-complete sum of weights. Default `FALSE`.
#' @param decimals Integer or `NULL`. If integer, rounds all numeric
#'   output columns to this many places. Default `NULL` (no rounding).
#' @param min_cell_n Integer. Minimum pairwise unweighted count before
#'   `surveycore_warning_small_cell` fires. Default `30L` (AAPOR).
#' @param na.rm Logical. If `TRUE` (default), pairwise-complete deletion
#'   per pair, and rows with `NA` in any group variable are excluded from
#'   the output. If `FALSE`, NAs propagate to produce `NaN` estimates;
#'   NA group values are retained as their own group row.
#' @param label_values Logical. If `TRUE` (default) and the grouping
#'   variable has value labels, the group column is converted to a
#'   labelled factor.
#' @param label_vars Logical. If `TRUE` (default) and variable labels are
#'   set in metadata, `var1` and `var2` show labels instead of raw names.
#' @param name_style `"surveycore"` (default) or `"broom"`. Under
#'   `"broom"`, renames `covariance` -> `estimate`, `se` -> `std.error`,
#'   `ci_low` -> `conf.low`, `ci_high` -> `conf.high`.
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
#' @return A `survey_covariance` tibble (also inheriting `survey_result`).
#'   Columns, in order:
#' \itemize{
#'   \item `[group_cols...]` — group variable columns (when active), first.
#'   \item `var1`, `var2` — factor columns identifying the pair (levels in
#'     `x`-supply order).
#'   \item `covariance` — design-based Pearson covariance estimate
#'     (Kish-corrected). `NaN` for degenerate cells; `0` for pairs where
#'     at least one variable is constant on the active domain.
#'   \item Uncertainty columns (`se`, `var`, `cv`, `ci_low`, `ci_high`,
#'     `moe`, `deff`) — only those requested via `variance`.
#'   \item `n` — pairwise unweighted count.
#'   \item `n_weighted` — pair's sum of weights (only when requested).
#' }
#'
#' @references
#'   Mood, A. M., Graybill, F. A., & Boes, D. C. (1974).
#'     *Introduction to the Theory of Statistics* (3rd ed.). McGraw-Hill.
#'
#'   Lumley, T. (2010). *Complex Surveys: A Guide to Analysis Using R*.
#'     Wiley.
#'
#'   Cochran, W. G. (1977). *Sampling Techniques* (3rd ed.). Wiley.
#'
#'   Demnati, A., & Rao, J. N. K. (2004). Linearization variance
#'     estimators for survey data. *Survey Methodology*, 30, 17--26.
#'
#' @examples
#' d <- as_survey(
#'   nhanes_2017,
#'   ids = sdmvpsu,
#'   weights = wtint2yr,
#'   strata = sdmvstra,
#'   nest = TRUE
#' )
#' get_covariance(d, x = c(ridageyr, bpxsy1))
#'
#' # Include the diagonal (self-pairs return Var(x), not 1)
#' get_covariance(d, x = c(ridageyr, bpxsy1), diagonal = TRUE)
#'
#' # With grouping
#' get_covariance(d, x = c(ridageyr, bpxsy1), group = riagendr)
#' @family analysis
#' @export
get_covariance <- function(
  design,
  x,
  group = NULL,
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
  ...,
  .id = NULL,
  .if_missing_var = NULL
) {
  if (S7::S7_inherits(design, survey_collection)) {
    return(.dispatch_over_collection(
      get_covariance,
      design,
      x = {{ x }},
      group = {{ group }},
      ...,
      .id = .id,
      .if_missing_var = .if_missing_var
    ))
  }

  # ── Step 1: Validate ────────────────────────────────────────────────────────
  .check_unsupported_class(design, "get_covariance")
  .validate_shared_args(
    variance,
    conf_level,
    name_style,
    decimals = decimals,
    na.rm = na.rm
  )

  # ── Step 2: Resolve variables ───────────────────────────────────────────────
  x_quo <- rlang::enquo(x)
  group_quo <- rlang::enquo(group)

  x_names_all <- .resolve_tidy_select(x_quo, design@data)

  # Empty-selection guard runs *before* numeric drop pass (matches get_corr).
  if (length(x_names_all) < 2L) {
    cli::cli_abort(
      c(
        "x" = paste0(
          "{.fn get_covariance} requires at least 2 variables, but ",
          "{.arg x} resolved to {length(x_names_all)} variable{?s}."
        )
      ),
      class = "surveycore_error_insufficient_variables"
    )
  }

  # Drop non-numeric variables with one CV-3 warning listing all dropped cols.
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
          "Dropped non-numeric variable{?s} from {.arg x}: ",
          "{.field {dropped}}."
        ),
        "i" = "{.fn get_covariance} requires numeric variables."
      ),
      class = "surveycore_warning_covariance_non_numeric"
    )
  }
  x_names <- x_names_all[is_numeric_col]

  if (length(x_names) < 2L) {
    cli::cli_abort(
      c(
        "x" = paste0(
          "{.fn get_covariance} requires at least 2 numeric variables, ",
          "but only {length(x_names)} numeric variable{?s} remain after ",
          "dropping non-numeric columns."
        )
      ),
      class = "surveycore_error_insufficient_variables"
    )
  }

  # ── Step 3: Domain mask ─────────────────────────────────────────────────────
  domain_mask <- .apply_domain(design)
  degf <- Inf # Normal approximation; matches get_variance() / svyvar()

  # ── Step 4: Variable metadata ───────────────────────────────────────────────
  x_meta_list <- lapply(x_names, function(nm) .extract_var_meta(design, nm))
  names(x_meta_list) <- x_names

  # ── Step 5: Display names (labels or raw names) ─────────────────────────────
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

  # ── Step 6: Resolve groups ──────────────────────────────────────────────────
  group_vars <- .resolve_groups(design, group_quo)

  # ── Step 7: Single-level warning per group var ──────────────────────────────
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

  # ── Step 8: Build group combinations ────────────────────────────────────────
  if (length(group_vars) > 0L) {
    domain_data <- design@data[domain_mask, group_vars, drop = FALSE]
    group_combos <- .build_group_combos(domain_data, na.rm)
    n_combos <- nrow(group_combos)
  } else {
    group_combos <- data.frame()
    n_combos <- 1L
  }

  # ── Step 9: Pair generation rules ───────────────────────────────────────────
  # Unique pair indices (i, j) with i < j (lower triangle), and self-pairs.
  p <- length(x_names)

  pair_keys <- character(0)
  pair_a <- integer(0)
  pair_b <- integer(0)

  # Lower-triangle off-diagonal pairs (i < j), supply order.
  for (i in seq_len(p - 1L)) {
    for (j in seq(i + 1L, p)) {
      pair_keys <- c(pair_keys, paste0(i, "-", j))
      pair_a <- c(pair_a, i)
      pair_b <- c(pair_b, j)
    }
  }
  # Self-pairs (i = j) — always available for diagonal expansion below.
  for (i in seq_len(p)) {
    pair_keys <- c(pair_keys, paste0(i, "-", i))
    pair_a <- c(pair_a, i)
    pair_b <- c(pair_b, i)
  }
  pair_lookup <- stats::setNames(seq_along(pair_keys), pair_keys)

  # Row expansion: which (i, j) ordered rows appear, given redundant/diagonal.
  row_i <- integer(0)
  row_j <- integer(0)
  for (i in seq_len(p - 1L)) {
    for (j in seq(i + 1L, p)) {
      row_i <- c(row_i, i)
      row_j <- c(row_j, j)
      if (isTRUE(redundant)) {
        row_i <- c(row_i, j)
        row_j <- c(row_j, i)
      }
    }
  }
  if (isTRUE(diagonal)) {
    row_i <- c(row_i, seq_len(p))
    row_j <- c(row_j, seq_len(p))
  }
  n_rows_per_combo <- length(row_i)

  # Determine which unique pairs we actually need to compute, given the
  # row expansion. This avoids computing self-pairs unless diagonal = TRUE.
  needed_keys <- unique(vapply(
    seq_along(row_i),
    function(k) {
      ii <- min(row_i[[k]], row_j[[k]])
      jj <- max(row_i[[k]], row_j[[k]])
      paste0(ii, "-", jj)
    },
    character(1L)
  ))

  # ── Step 10: Outer combo / inner pair compute loops ─────────────────────────
  all_pair_results <- vector("list", n_combos)
  all_grp_rows <- vector("list", n_combos)
  small_cell_ns <- integer(0)
  all_na_pairs <- list()
  insuff_n_pairs <- list()

  for (ci_idx in seq_len(n_combos)) {
    if (length(group_vars) > 0L) {
      combo_row <- group_combos[ci_idx, , drop = FALSE]
      data_cols <- as.list(design@data[group_vars])
      group_match <- .match_group_combo(data_cols, combo_row)
      active_mask <- domain_mask & group_match
      all_grp_rows[[ci_idx]] <- combo_row
    } else {
      active_mask <- domain_mask
    }
    active_domain <- as.numeric(active_mask)

    pair_results_this <- vector("list", length(pair_keys))
    names(pair_results_this) <- pair_keys

    for (key in needed_keys) {
      idx <- pair_lookup[[key]]
      i <- pair_a[[idx]]
      j <- pair_b[[idx]]
      xnm <- x_names[[i]]
      ynm <- x_names[[j]]
      res <- .covariance_pair_result(
        design,
        xnm,
        ynm,
        active_domain,
        na.rm
      )

      # Apply na.rm = FALSE semantics: when NAs are present in either
      # variable on the active domain, NaN-out the estimates and use
      # active-mask count (pre-NA filter) for n.
      if (!isTRUE(na.rm)) {
        n_active_rows <- as.integer(sum(active_mask))
        any_na <- any(
          is.na(design@data[[xnm]][active_mask]) |
            is.na(design@data[[ynm]][active_mask])
        )
        res$n <- n_active_rows
        if (any_na) {
          res$covariance <- NaN
          res$se <- NaN
          res$se_srs <- NaN
        }
      }

      pair_results_this[[key]] <- res

      # Per-pair edge-case warning bookkeeping (only with na.rm = TRUE).
      if (isTRUE(na.rm)) {
        n_active_rows <- as.integer(sum(active_mask))
        if (res$n == 0L && n_active_rows > 0L) {
          all_na_pairs[[length(all_na_pairs) + 1L]] <- c(xnm, ynm)
        } else if (res$n == 1L) {
          insuff_n_pairs[[length(insuff_n_pairs) + 1L]] <- c(xnm, ynm)
        }
      }

      if (!is.na(res$n) && res$n > 0L && res$n < min_cell_n) {
        small_cell_ns <- c(small_cell_ns, res$n)
      }
    }

    all_pair_results[[ci_idx]] <- pair_results_this
  }

  # ── Step 11: Per-pair edge-case warnings (deduplicated) ─────────────────────
  if (length(all_na_pairs) > 0L) {
    seen <- character(0)
    for (pp in all_na_pairs) {
      key <- paste(pp, collapse = "\t")
      if (key %in% seen) {
        next
      }
      seen <- c(seen, key)
      x_var <- pp[[1L]]
      y_var <- pp[[2L]]
      cli::cli_warn(
        c(
          "!" = paste0(
            "Pair ({.field {x_var}}, {.field {y_var}}) is all-",
            "{.code NA} on the active domain. Returning {.code NaN} ",
            "with {.code n = 0}."
          )
        ),
        class = "surveycore_warning_covariance_all_na"
      )
    }
  }
  if (length(insuff_n_pairs) > 0L) {
    seen <- character(0)
    for (pp in insuff_n_pairs) {
      key <- paste(pp, collapse = "\t")
      if (key %in% seen) {
        next
      }
      seen <- c(seen, key)
      x_var <- pp[[1L]]
      y_var <- pp[[2L]]
      cli::cli_warn(
        c(
          "!" = paste0(
            "Pair ({.field {x_var}}, {.field {y_var}}) has 1 ",
            "pairwise-complete observation in the active domain; ",
            "covariance requires at least 2. Returning {.code NaN}."
          )
        ),
        class = "surveycore_warning_covariance_insufficient_n"
      )
    }
  }

  # ── Step 12: Small-cell warning ─────────────────────────────────────────────
  n_small <- length(small_cell_ns)
  if (n_small > 0L) {
    cli::cli_warn(
      c(
        "!" = paste0(
          "{n_small} cell{?s} {?has/have} fewer than {min_cell_n} ",
          "unweighted observations. Estimates in these cells may be ",
          "unreliable for public reporting (AAPOR guidance)."
        )
      ),
      class = "surveycore_warning_small_cell"
    )
  }

  # ── Step 13: Pre-allocate accumulator vectors ───────────────────────────────
  total_rows <- n_combos * n_rows_per_combo
  acc_var1 <- character(total_rows)
  acc_var2 <- character(total_rows)
  acc_cov <- numeric(total_rows)
  acc_se <- numeric(total_rows)
  acc_sesrs <- numeric(total_rows)
  acc_n <- integer(total_rows)
  acc_nw <- numeric(total_rows)
  acc_grp_rows <- vector("list", n_combos)

  # ── Step 14: Combo fill loop ────────────────────────────────────────────────
  for (ci_idx in seq_len(n_combos)) {
    offset <- (ci_idx - 1L) * n_rows_per_combo
    pair_results <- all_pair_results[[ci_idx]]

    for (ri in seq_len(n_rows_per_combo)) {
      i <- row_i[[ri]]
      j <- row_j[[ri]]
      out <- offset + ri

      acc_var1[[out]] <- display_names[[x_names[[i]]]]
      acc_var2[[out]] <- display_names[[x_names[[j]]]]

      ii <- min(i, j)
      jj <- max(i, j)
      key <- paste0(ii, "-", jj)
      res <- pair_results[[key]]

      acc_cov[[out]] <- res$covariance
      acc_se[[out]] <- res$se
      acc_sesrs[[out]] <- res$se_srs
      acc_n[[out]] <- as.integer(res$n)
      acc_nw[[out]] <- res$n_weighted
    }

    if (length(group_vars) > 0L) {
      grp_block <- all_grp_rows[[ci_idx]][
        rep(1L, n_rows_per_combo),
        ,
        drop = FALSE
      ]
      acc_grp_rows[[ci_idx]] <- grp_block
    }
  }

  # ── Step 15: Build groups_df ────────────────────────────────────────────────
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

  # ── Step 16: Build variance columns ─────────────────────────────────────────
  var_cols <- .add_variance_cols(
    se_vec = acc_se,
    estimate_vec = acc_cov,
    se_srs_vec = acc_sesrs,
    conf_level = conf_level,
    degf = degf,
    variance = variance
  )

  # 0 / 0 deff guard: per-row override. Where both se and se_srs are exactly
  # 0 (constant-variable pairs), report deff = 0 (not NA).
  if (!is.null(variance) && "deff" %in% variance) {
    is_zero_zero <- !is.na(acc_se) &
      !is.na(acc_sesrs) &
      acc_se == 0 &
      acc_sesrs == 0
    if (any(is_zero_zero)) {
      var_cols$deff[is_zero_zero] <- 0
    }
  }

  # ── Step 17: Assemble col_vecs ──────────────────────────────────────────────
  col_vecs <- list(
    var1 = acc_var1,
    var2 = acc_var2,
    covariance = acc_cov
  )
  col_vecs <- c(col_vecs, var_cols)
  col_vecs$n <- acc_n
  if (isTRUE(n_weighted)) {
    col_vecs$n_weighted <- acc_nw
  }

  # ── Step 18: Build meta_args ────────────────────────────────────────────────
  group_meta <- .build_group_meta(design, group_vars)

  design_class <- class(design)[[1L]]

  meta_args <- list(
    conf_level = conf_level,
    call = match.call(),
    method = "covariance",
    group = group_meta,
    x = x_meta_list,
    design_class = design_class,
    name_style = name_style,
    min_cell_n = min_cell_n,
    redundant = redundant,
    diagonal = diagonal,
    na_rm = isTRUE(na.rm)
  )

  # ── Step 19: Assemble result tibble ─────────────────────────────────────────
  result <- .make_result_tibble(
    col_vecs,
    groups_df,
    "survey_covariance",
    design,
    meta_args,
    COVARIANCE_META_KEYS
  )

  # ── Step 20: Convert var1/var2 to factors (supply order) ────────────────────
  uniq_display <- unique(display_names)
  result$var1 <- factor(result$var1, levels = uniq_display)
  result$var2 <- factor(result$var2, levels = uniq_display)

  # ── Step 21: Apply decimals and name style ──────────────────────────────────
  if (!is.null(decimals)) {
    result <- .apply_decimals(result, decimals)
  }
  result <- .apply_name_style(result, name_style)

  # ── Step 22: Attach column-level label attributes ───────────────────────────
  result <- .attach_covariance_labels(
    result,
    conf_level,
    name_style,
    group_vars,
    design
  )

  result
}
