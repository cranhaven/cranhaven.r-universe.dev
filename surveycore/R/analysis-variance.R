# R/analysis-variance.R
#
# Design-based finite-population variance estimation for survey designs.
# Exported function: get_variance()
#
# Internal helpers: analysis-variance-helpers.R

# ── get_variance() ────────────────────────────────────────────────────────────

#' Design-Based Population Variance for a Survey Design
#'
#' Compute the design-based estimate of the finite-population variance for one
#' or more numeric variables in a survey design, with optional grouping,
#' uncertainty quantification, and metadata-driven labelling. Matches
#' `survey::svyvar()` numerically (Kish `n/(n-1)` correction) on Taylor,
#' replicate, twophase, and nonprob designs.
#'
#' @details
#' Confidence intervals use the normal-Wald approximation on the SE of the
#' variance estimate: `ci_low = variance - z * se`,
#' `ci_high = variance + z * se`, where `z = qnorm((1 + conf_level) / 2)`.
#' The bounds are **not clamped**. When the true variance is near zero with
#' wide SE, `ci_low` may be negative. Users who want non-negative lower
#' bounds can clamp at `0` post-hoc. This behaviour matches
#' `survey::svyvar()`.
#'
#' Under `na_handling = "pairwise"` (the default), each focal variable
#' contributes its own per-variable complete-case count to `n`. Under
#' `na_handling = "listwise"`, every output row shares the intersection
#' complete-case count — rows with `NA` in any selected variable are
#' excluded from every variable's calculation.
#'
#' @param design A survey design object: `survey_taylor`, `survey_replicate`,
#'   `survey_twophase`, or `survey_nonprob`. Also accepts a
#'   [`survey_collection`].
#' @param x <[`tidy-select`][tidyselect::language]> One or more unquoted
#'   numeric variable names. Must resolve to at least one numeric column;
#'   non-numeric columns are rejected (no silent drop).
#' @param group <[`tidy-select`][tidyselect::language]> Optional grouping
#'   variable(s). Combined with any grouping set by `group_by()`. Default
#'   `NULL`.
#' @param variance `NULL` or a character vector of one or more of `"se"`,
#'   `"ci"`, `"var"`, `"cv"`, `"moe"`, `"deff"`. Controls which uncertainty
#'   columns appear in the output. Default `"ci"`.
#' @param conf_level Numeric scalar in (0, 1). Confidence level for intervals.
#'   Default `0.95`.
#' @param n_weighted Logical. If `TRUE`, add an `n_weighted` column with the
#'   sum of weights for non-NA, positive-weight observations in each row's
#'   estimate. Default `FALSE`.
#' @param decimals Integer or `NULL`. If an integer, rounds all numeric output
#'   columns to this many decimal places. Default `NULL` (no rounding).
#' @param min_cell_n Integer. Minimum unweighted cell count before
#'   `surveycore_warning_small_cell` fires. Default `30L` (AAPOR guidance).
#' @param na.rm Logical. If `TRUE` (default), `NA` values in the focal
#'   variable are excluded from the estimate and rows with `NA` in any
#'   grouping variable are excluded from the output. If `FALSE`, `NA`
#'   propagates to produce `NaN` estimates.
#' @param na_handling `"pairwise"` (default) or `"listwise"`. In
#'   multi-variable mode controls whether each focal variable uses its own
#'   complete-case set (`"pairwise"`) or the intersection across all focal
#'   variables (`"listwise"`). Ignored when `na.rm = FALSE`.
#' @param label_values Logical. Accepted for API consistency across `get_*()`
#'   functions. Used to convert grouping-variable codes to value labels.
#'   Default `TRUE`.
#' @param label_vars Logical. If `TRUE` (default), the `name` column
#'   shows variable labels when available (falling back to raw names).
#' @param name_style `"surveycore"` (default) or `"broom"`. Under `"broom"`,
#'   renames `variance` → `estimate`, `se` → `std.error`, `ci_low` →
#'   `conf.low`, `ci_high` → `conf.high`.
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
#' @return A `survey_variance` tibble (also inheriting `survey_result`).
#'   Columns, in order:
#' \itemize{
#'   \item `[.id]` — survey identifier column, only when `design` is a
#'     [`survey_collection`].
#'   \item `[group_cols...]` — group variable columns (when active), first.
#'   \item `name` — focal variable name (or its label when `label_vars = TRUE`).
#'   \item `variance` — design-based point estimate of the finite-population
#'     variance. Note: the column is always named `variance` regardless of the
#'     `variance` parameter (which controls uncertainty columns, not this
#'     column). `NaN` for degenerate cells; exact `0` for constant-in-domain
#'     variables.
#'   \item Uncertainty columns (`se`, `var`, `cv`, `ci_low`, `ci_high`,
#'     `moe`, `deff`) — only those requested via the `variance` parameter.
#'     The `var` uncertainty column is the variance of the estimated variance,
#'     distinct from the `variance` point estimate column.
#'   \item `n` — unweighted count of non-NA observations used.
#'   \item `n_weighted` — sum of weights (only when `n_weighted = TRUE`).
#' }
#'
#' @examples
#' d <- as_survey(
#'   nhanes_2017,
#'   ids = sdmvpsu,
#'   weights = wtint2yr,
#'   strata = sdmvstra,
#'   nest = TRUE
#' )
#' get_variance(d, ridageyr)
#'
#' # Multiple variables
#' get_variance(d, c(ridageyr, bpxsy1))
#'
#' # With grouping
#' get_variance(d, ridageyr, group = riagendr)
#' @family analysis
#' @export
get_variance <- function(
  design,
  x,
  group = NULL,
  variance = "ci",
  conf_level = 0.95,
  n_weighted = FALSE,
  decimals = NULL,
  min_cell_n = 30L,
  na.rm = TRUE,
  na_handling = c("pairwise", "listwise"),
  label_values = TRUE,
  label_vars = TRUE,
  name_style = "surveycore",
  ...,
  .id = NULL,
  .if_missing_var = NULL
) {
  if (S7::S7_inherits(design, survey_collection)) {
    return(.dispatch_over_collection(
      get_variance,
      design,
      x = {{ x }},
      group = {{ group }},
      ...,
      .id = .id,
      .if_missing_var = .if_missing_var
    ))
  }

  # ── Step 1: Validate ────────────────────────────────────────────────────────
  .check_unsupported_class(design, "get_variance")
  .validate_shared_args(
    variance,
    conf_level,
    name_style,
    decimals = decimals,
    na.rm = na.rm
  )
  na_handling <- match.arg(na_handling)

  # ── Step 2: Resolve variables, groups, domain ───────────────────────────────
  x_quo <- rlang::enquo(x)
  group_quo <- rlang::enquo(group)

  var_names <- .resolve_tidy_select(x_quo, design@data)

  if (length(var_names) == 0L) {
    cli::cli_abort(
      c(
        "x" = "{.fn get_variance} requires at least one variable.",
        "i" = "{.arg x} resolved to 0 variables."
      ),
      class = "surveycore_error_wrong_variable_count"
    )
  }

  for (vn in var_names) {
    vcol <- design@data[[vn]]
    if (!is.numeric(vcol)) {
      cli::cli_abort(
        c(
          "x" = "{.arg x} must be numeric, not {.cls {class(vcol)}}.",
          "i" = "Column {.field {vn}} cannot be used with {.fn get_variance}."
        ),
        class = "surveycore_error_non_numeric_variable"
      )
    }
  }

  group_vars <- .resolve_groups(design, group_quo)
  domain_mask <- .apply_domain(design)
  degf <- Inf # Normal approximation; matches survey::svyvar() default

  # ── Step 3: Single-level warning for group variables ────────────────────────
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

  # ── Step 4: Build group combinations ────────────────────────────────────────
  if (length(group_vars) > 0L) {
    domain_data <- design@data[domain_mask, group_vars, drop = FALSE]
    group_combos <- .build_group_combos(domain_data, na.rm)
    n_combos <- nrow(group_combos)
  } else {
    group_combos <- data.frame()
    n_combos <- 1L
  }

  # ── Step 5: Pre-collect variable metadata ───────────────────────────────────
  x_meta_list <- lapply(var_names, function(vn) .extract_var_meta(design, vn))
  names(x_meta_list) <- var_names

  # ── Step 6: Main accumulation loop ──────────────────────────────────────────
  acc_name <- character(0)
  acc_variance <- numeric(0)
  acc_se <- numeric(0)
  acc_sesrs <- numeric(0)
  acc_n <- integer(0)
  acc_nw <- numeric(0)
  acc_grp_rows <- vector("list", 0L)

  small_cell_ns <- integer(0)
  all_na_vars <- character(0)
  insuff_n_vars <- character(0)

  # Precompute listwise shared active mask (NA across any selected variable)
  # when na_handling == "listwise" AND na.rm == TRUE
  use_listwise <- identical(na_handling, "listwise") && isTRUE(na.rm)
  if (use_listwise) {
    any_na <- Reduce(
      `|`,
      lapply(var_names, function(vn) is.na(design@data[[vn]]))
    )
    listwise_nonna <- !any_na
  } else {
    listwise_nonna <- rep(TRUE, nrow(design@data))
  }

  for (vn in var_names) {
    x_col <- design@data[[vn]]

    # Display name for the "name" column
    vn_display <- if (
      label_vars && !is.null(x_meta_list[[vn]]$variable_label)
    ) {
      x_meta_list[[vn]]$variable_label
    } else {
      vn
    }

    for (ci_idx in seq_len(n_combos)) {
      if (length(group_vars) > 0L) {
        combo_row <- group_combos[ci_idx, , drop = FALSE]
        data_cols <- as.list(design@data[group_vars])
        group_match <- .match_group_combo(data_cols, combo_row)
        active_mask <- domain_mask & group_match
      } else {
        active_mask <- domain_mask
      }

      # Apply na.rm / na_handling to build the 0/1 domain vector
      if (isTRUE(na.rm)) {
        if (use_listwise) {
          domain_vec <- as.numeric(active_mask & listwise_nonna)
        } else {
          domain_vec <- as.numeric(active_mask & !is.na(x_col))
        }
      } else {
        domain_vec <- as.numeric(active_mask)
      }

      cell <- .variance_cell(design, vn, domain_vec)

      # When na.rm = FALSE, n reports the size of the active domain
      # (pre-NA filter), not the complete-case count. Estimate is NaN
      # whenever any NA exists in the active domain because the weighted
      # mean propagates NaN.
      if (!isTRUE(na.rm)) {
        cell$n <- as.integer(sum(active_mask))
        if (any(is.na(design@data[[vn]][active_mask]))) {
          cell$variance <- NaN
          cell$se <- NaN
          cell$se_srs <- NaN
        }
      }

      # Emit per-variable warnings for edge-case n
      # Detect "all-NA in active domain": active rows exist but post-domain
      # n_d is 0.
      n_active_rows <- sum(active_mask)
      if (
        isTRUE(na.rm) &&
          cell$n == 0L &&
          n_active_rows > 0L
      ) {
        all_na_vars <- c(all_na_vars, vn)
      } else if (isTRUE(na.rm) && cell$n == 1L) {
        insuff_n_vars <- c(insuff_n_vars, vn)
      }

      if (!is.na(cell$n) && cell$n > 0L && cell$n < min_cell_n) {
        small_cell_ns <- c(small_cell_ns, cell$n)
      }

      acc_name <- c(acc_name, vn_display)
      acc_variance <- c(acc_variance, cell$variance)
      acc_se <- c(acc_se, cell$se)
      acc_sesrs <- c(acc_sesrs, cell$se_srs)
      acc_n <- c(acc_n, cell$n)
      acc_nw <- c(acc_nw, cell$n_weighted)

      if (length(group_vars) > 0L) {
        acc_grp_rows <- c(acc_grp_rows, list(combo_row))
      }
    }
  }

  # ── Step 7: Variance-specific warnings ──────────────────────────────────────
  all_na_vars <- unique(all_na_vars)
  if (length(all_na_vars) > 0L) {
    for (vn in all_na_vars) {
      cli::cli_warn(
        c(
          "!" = paste0(
            "All values of {.field {vn}} are {.code NA} in the active domain. ",
            "Returning {.code NaN} with {.code n = 0}."
          )
        ),
        class = "surveycore_warning_variance_all_na"
      )
    }
  }
  insuff_n_vars <- unique(insuff_n_vars)
  if (length(insuff_n_vars) > 0L) {
    for (vn in insuff_n_vars) {
      cli::cli_warn(
        c(
          "!" = paste0(
            "{.field {vn}} has 1 non-{.code NA} observation in the active ",
            "domain; variance requires at least 2. Returning {.code NaN}."
          )
        ),
        class = "surveycore_warning_variance_insufficient_n"
      )
    }
  }

  # ── Step 8: Small-cell warning ──────────────────────────────────────────────
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

  # ── Step 9: Build column vectors ────────────────────────────────────────────
  var_cols <- .add_variance_cols(
    se_vec = acc_se,
    estimate_vec = acc_variance,
    se_srs_vec = acc_sesrs,
    conf_level = conf_level,
    degf = degf,
    variance = variance
  )

  col_vecs <- list()
  col_vecs$name <- acc_name
  col_vecs$variance <- acc_variance
  col_vecs <- c(col_vecs, var_cols)
  col_vecs$n <- acc_n

  if (isTRUE(n_weighted)) {
    col_vecs$n_weighted <- acc_nw
  }

  # ── Step 10: Build groups_df ────────────────────────────────────────────────
  if (length(group_vars) > 0L && length(acc_grp_rows) > 0L) {
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

  # ── Step 11: Build meta_args ────────────────────────────────────────────────
  group_meta <- .build_group_meta(design, group_vars)

  design_class <- class(design)[[1L]]

  meta_args <- list(
    conf_level = conf_level,
    call = match.call(),
    group = group_meta,
    x = x_meta_list,
    design_class = design_class,
    name_style = name_style,
    min_cell_n = min_cell_n,
    na_handling = na_handling
  )

  # ── Step 12: Assemble result ────────────────────────────────────────────────
  result <- .make_result_tibble(
    col_vecs,
    groups_df,
    "survey_variance",
    design,
    meta_args,
    VARIANCE_META_KEYS
  )

  # ── Step 13: Apply decimals and name style ──────────────────────────────────
  if (!is.null(decimals)) {
    result <- .apply_decimals(result, decimals)
  }
  result <- .apply_name_style(result, name_style)

  # ── Step 14: Attach column-level label attributes ───────────────────────────
  result <- .attach_variance_labels(
    result,
    conf_level,
    name_style,
    group_vars,
    design
  )

  result
}


# ── .attach_variance_labels() ─────────────────────────────────────────────────
#
# Attach `label` attributes to every column in a survey_variance result
# tibble, for gt integration. Preserves the .meta attribute and S3 class.
#
# @param result        A survey_variance tibble (post-name-style rename).
# @param conf_level    Numeric scalar. Used to interpolate CI bound labels.
# @param name_style    Character(1). "surveycore" or "broom" — determines
#                      the actual column names in `result`.
# @param group_vars    Character vector of group variable names (raw).
# @param design        The survey design object (used for group-var labels).
# @return The tibble with labels attached to every column, unchanged
#         otherwise.
.attach_variance_labels <- function(
  result,
  conf_level,
  name_style,
  group_vars,
  design
) {
  saved_meta <- attr(result, ".meta")
  saved_class <- class(result)

  conf_pct <- formatC(100 * conf_level, digits = 0, format = "f")

  # Canonical label map keyed by surveycore column names
  lbl_map_surveycore <- c(
    name = "Variable",
    variance = "Variance",
    se = "SE",
    var = "Variance of estimate",
    ci_low = paste0(conf_pct, "% CI low"),
    ci_high = paste0(conf_pct, "% CI high"),
    cv = "CV",
    moe = "Margin of error",
    deff = "Design effect",
    n = "N",
    n_weighted = "Weighted N"
  )

  # Broom renames: variance → estimate, se → std.error, ci_low → conf.low,
  # ci_high → conf.high. Build a matching map keyed by broom column names.
  if (identical(name_style, "broom")) {
    lbl_map <- c(
      name = "Variable",
      estimate = "Variance",
      std.error = "SE",
      var = "Variance of estimate",
      conf.low = paste0(conf_pct, "% CI low"),
      conf.high = paste0(conf_pct, "% CI high"),
      cv = "CV",
      moe = "Margin of error",
      deff = "Design effect",
      n = "N",
      n_weighted = "Weighted N"
    )
  } else {
    lbl_map <- lbl_map_surveycore
  }

  for (nm in names(result)) {
    if (nm %in% names(lbl_map)) {
      attr(result[[nm]], "label") <- lbl_map[[nm]]
    } else if (nm %in% group_vars) {
      gv_label <- design@metadata@variable_labels[[nm]] %||%
        attr(design@data[[nm]], "label", exact = TRUE) %||%
        nm
      attr(result[[nm]], "label") <- gv_label
    }
  }

  attr(result, ".meta") <- saved_meta
  class(result) <- saved_class
  result
}
