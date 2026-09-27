# R/analysis-ratios.R
#
# Survey-weighted ratio estimation via delta method or direct per-replicate.
# Exported function: get_ratios()
#
# Internal helpers: analysis-ratios-helpers.R

# ── get_ratios() ──────────────────────────────────────────────────────────────

#' Survey-Weighted Ratio Estimation
#'
#' Estimate the ratio of two survey-weighted totals (numerator / denominator)
#' for a survey design object. Uses the delta method (linearization) for
#' variance estimation for Taylor, SRS, calibrated, and two-phase designs, and
#' direct per-replicate computation for replicate-weight designs. Both
#' approaches are equivalent to `survey::svyratio()` for their respective
#' design types.
#' Supports optional grouping, domain estimation, and all five survey design
#' classes.
#'
#' @param design A survey design object: `survey_taylor`, `survey_replicate`,
#'   `survey_twophase`, or `survey_nonprob`.
#' @param numerator <[`tidy-select`][tidyselect::language]> A single unquoted
#'   numeric variable name for the numerator. Must resolve to exactly one
#'   numeric column.
#' @param denominator <[`tidy-select`][tidyselect::language]> A single unquoted
#'   numeric variable name for the denominator. Must resolve to exactly one
#'   numeric column. All in-domain values must not sum to zero.
#' @param group <[`tidy-select`][tidyselect::language]> Optional grouping
#'   variable(s). Combined with any grouping set by `group_by()`. Rows where
#'   the grouping variable is `NA` are excluded from all groups and do not
#'   appear in the output. This matches `dplyr::group_by()` semantics.
#'   Default `NULL`.
#' @param variance `NULL` or a character vector from `"se"`, `"ci"`, `"var"`,
#'   `"cv"`, `"moe"`, `"deff"`. Controls which uncertainty columns appear in
#'   the output. Default `"ci"`.
#' @param conf_level Numeric scalar in (0, 1). Confidence level for
#'   confidence intervals. Default `0.95`.
#' @param n_weighted Logical. If `TRUE`, add an `n_weighted` column with the
#'   sum of weights for rows where both numerator and denominator are non-NA
#'   in each group. Default `FALSE`.
#' @param decimals Integer or `NULL`. If an integer, rounds all numeric output
#'   columns (e.g., `ratio`, `se`, `ci_low`, `ci_high`) to this many decimal
#'   places. Default `NULL` (no rounding).
#' @param min_cell_n Integer. Minimum unweighted cell count before
#'   `surveycore_warning_small_cell` fires. Default `30L` (AAPOR guidance).
#' @param na.rm Logical. If `TRUE` (default), `NA` values are excluded from
#'   analysis: observations where the analysis variable is `NA` are dropped
#'   from calculations, and observations where any group variable is `NA` are
#'   excluded from the output. If `FALSE`, `NA` observations in the analysis
#'   variable are included in calculations, and observations where a group
#'   variable is `NA` are collected into their own group row in the output
#'   (appearing after all non-`NA` group rows).
#' @param label_values Logical. Accepted for API consistency across `get_*()`
#'   functions. For `get_ratios()`, no value-level cells appear in the output,
#'   so this parameter has no effect. Default `TRUE`.
#' @param label_vars Logical. Accepted for API uniformity; has no visible
#'   effect on `get_ratios()` output. Default `TRUE`.
#' @param name_style `"surveycore"` (default) or `"broom"`. When `"broom"`,
#'   renames `ratio` → `estimate`, `se` → `std.error`, `ci_low` →
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
#' @return A `survey_ratios` tibble (also inheriting `survey_result`).
#' \itemize{
#'   \item `[group_cols...]` — group variable columns (when active), first.
#'   \item `ratio` — estimated ratio (weighted total of numerator / weighted
#'     total of denominator).
#'   \item Variance columns (`se`, `var`, `cv`, `ci_low`, `ci_high`, `moe`,
#'     `deff`) — only those requested via `variance`.
#'   \item `n` — unweighted count of rows where both numerator and denominator
#'     are non-NA.
#'   \item `n_weighted` — sum of weights (only when requested).
#' }
#' Numerator and denominator variable names are stored in `meta(result)`, not
#' as output columns. Use `meta(result)$numerator` and
#' `meta(result)$denominator` to access them.
#'
#' @examples
#' d <- as_survey(pew_npors_2025, weights = weight, strata = stratum)
#'
#' # Ratio of prayer frequency to in-person attendance frequency
#' get_ratios(d, numerator = pray, denominator = attendper)
#'
#' # With grouped estimates
#' get_ratios(d, pray, attendper, group = gender)
#'
#' # AAPOR-compliant output
#' get_ratios(d, pray, attendper, variance = c("ci", "moe"), n_weighted = TRUE)
#' @family analysis
#' @export
get_ratios <- function(
  design,
  numerator,
  denominator,
  group = NULL,
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
      get_ratios,
      design,
      numerator = {{ numerator }},
      denominator = {{ denominator }},
      group = {{ group }},
      ...,
      .id = .id,
      .if_missing_var = .if_missing_var
    ))
  }
  # ── Step 1: Validate ────────────────────────────────────────────────────────
  .check_unsupported_class(design, "get_ratios")
  .validate_shared_args(
    variance,
    conf_level,
    name_style,
    decimals = decimals,
    na.rm = na.rm
  )

  # ── Step 2: Resolve variables, groups, domain ─────────────────────────────
  num_quo <- rlang::enquo(numerator)
  denom_quo <- rlang::enquo(denominator)
  group_quo <- rlang::enquo(group)

  num_names <- .resolve_tidy_select(num_quo, design@data)
  denom_names <- .resolve_tidy_select(denom_quo, design@data)

  if (length(num_names) != 1L) {
    cli::cli_abort(
      c(
        "x" = paste0(
          "{.fn get_ratios} requires exactly one ",
          "{.arg numerator} variable."
        ),
        "i" = "{.arg numerator} resolved to {length(num_names)} variable{?s}."
      ),
      class = "surveycore_error_wrong_variable_count"
    )
  }

  if (length(denom_names) != 1L) {
    cli::cli_abort(
      c(
        "x" = paste0(
          "{.fn get_ratios} requires exactly one {.arg denominator} variable."
        ),
        "i" = paste0(
          "{.arg denominator} resolved to {length(denom_names)} variable{?s}."
        )
      ),
      class = "surveycore_error_wrong_variable_count"
    )
  }

  num_name <- num_names[[1L]]
  denom_name <- denom_names[[1L]]
  y_col <- design@data[[num_name]]
  x_col <- design@data[[denom_name]]

  if (!is.numeric(y_col)) {
    cli::cli_abort(
      c(
        "x" = "{.arg numerator} must be numeric, not {.cls {class(y_col)}}.",
        "i" = "Column {.field {num_name}} cannot be used with {.fn get_ratios}."
      ),
      class = "surveycore_error_non_numeric_variable"
    )
  }

  if (!is.numeric(x_col)) {
    cli::cli_abort(
      c(
        "x" = "{.arg denominator} must be numeric, not {.cls {class(x_col)}}.",
        "i" = paste0(
          "Column {.field {denom_name}} cannot be used with {.fn get_ratios}."
        )
      ),
      class = "surveycore_error_non_numeric_variable"
    )
  }

  group_vars <- .resolve_groups(design, group_quo)
  domain_mask <- .apply_domain(design)
  degf <- Inf # Normal approximation; matches survey::svyratio() default

  # Flag for replicate dispatch (also true for survey_nonprob with repweights)
  is_replicate <- S7::S7_inherits(design, survey_replicate) ||
    (S7::S7_inherits(design, survey_nonprob) &&
      !is.null(design@variables$repweights))

  # ── Step 3: Single-level warning for group variables ──────────────────────
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

  # ── Step 4: Build group combinations ──────────────────────────────────────
  if (length(group_vars) > 0L) {
    domain_data <- design@data[domain_mask, group_vars, drop = FALSE]
    group_combos <- .build_group_combos(domain_data, na.rm)
    n_combos <- nrow(group_combos)
  } else {
    group_combos <- data.frame()
    n_combos <- 1L
  }

  # ── Step 5: Collect variable metadata ─────────────────────────────────────
  num_meta <- .extract_var_meta(design, num_name)
  denom_meta <- .extract_var_meta(design, denom_name)

  # ── Step 6: Main accumulation loop ────────────────────────────────────────
  acc_ratio <- numeric(0)
  acc_se <- numeric(0)
  acc_se_srs <- numeric(0)
  acc_n <- integer(0)
  acc_nw <- numeric(0)
  acc_grp_rows <- vector("list", 0L)

  small_cell_ns <- integer(0)
  tmp_col <- "..surveycore_ratio_lin.."

  for (ci in seq_len(n_combos)) {
    if (length(group_vars) > 0L) {
      combo_row <- group_combos[ci, , drop = FALSE]
      data_cols <- as.list(design@data[group_vars])
      group_match <- .match_group_combo(data_cols, combo_row)
      active_mask <- domain_mask & group_match
    } else {
      active_mask <- domain_mask
    }

    # n: count of rows where BOTH numerator AND denominator are non-NA
    na_either <- is.na(y_col) | is.na(x_col)
    n_cell <- as.integer(sum(active_mask & !na_either))

    # Small-cell check
    if (n_cell > 0L && n_cell < min_cell_n) {
      small_cell_ns <- c(small_cell_ns, n_cell)
    }

    # Build 0/1 domain vector based on na.rm setting
    if (na.rm) {
      domain_vec <- as.numeric(active_mask & !na_either)
    } else {
      domain_vec <- as.numeric(active_mask)
    }

    # Compute denominator total (for zero check + ratio point estimate)
    cell_x <- .total_cell(design, denom_name, domain_vec)
    total_x <- cell_x$total

    # All denominator values zero → error
    if (!is.na(total_x) && total_x == 0 && n_cell > 0L) {
      cli::cli_abort(
        c(
          "x" = paste0(
            "All values of the denominator ({.field {denom_name}}) are zero. ",
            "Cannot compute ratio."
          ),
          "i" = paste0(
            "Provide a denominator variable with at least ",
            "one non-zero value."
          )
        ),
        class = "surveycore_error_ratio_zero_denominator"
      )
    }

    # Empty cell or NA propagation (na.rm = FALSE with NAs present)
    if (is.na(total_x)) {
      acc_ratio <- c(acc_ratio, NA_real_)
      acc_se <- c(acc_se, NA_real_)
      acc_se_srs <- c(acc_se_srs, NA_real_)
      acc_n <- c(acc_n, n_cell)
      acc_nw <- c(acc_nw, NA_real_)
      if (length(group_vars) > 0L) {
        acc_grp_rows <- c(acc_grp_rows, list(combo_row))
      }
      next
    }

    # Compute numerator total and ratio estimate
    cell_y <- .total_cell(design, num_name, domain_vec)
    total_y <- cell_y$total
    ratio_est <- total_y / total_x

    # SE dispatch: replicate uses direct per-replicate ratios;
    # all other classes use the delta method via .total_cell().
    if (is_replicate) {
      se_result <- .replicate_ratio_cell(
        design,
        y_col,
        x_col,
        domain_vec,
        ratio_est,
        total_x
      )
    } else {
      se_result <- .delta_ratio_cell(
        design,
        y_col,
        x_col,
        domain_vec,
        ratio_est,
        total_x,
        tmp_col
      )
    }

    acc_ratio <- c(acc_ratio, ratio_est)
    acc_se <- c(acc_se, se_result$se)
    acc_se_srs <- c(acc_se_srs, se_result$se_srs)
    acc_n <- c(acc_n, n_cell)
    acc_nw <- c(acc_nw, cell_x$n_weighted)

    if (length(group_vars) > 0L) {
      acc_grp_rows <- c(acc_grp_rows, list(combo_row))
    }
  }

  # ── Step 7: Small-cell warning ─────────────────────────────────────────────
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

  # ── Step 8: Build variance columns ────────────────────────────────────────
  var_cols <- .add_variance_cols(
    se_vec = acc_se,
    estimate_vec = acc_ratio,
    se_srs_vec = acc_se_srs,
    conf_level = conf_level,
    degf = degf,
    variance = variance
  )

  # ── Step 9: Assemble col_vecs ─────────────────────────────────────────────
  col_vecs <- list()
  col_vecs$ratio <- acc_ratio
  col_vecs <- c(col_vecs, var_cols)
  col_vecs$n <- acc_n

  if (isTRUE(n_weighted)) {
    col_vecs$n_weighted <- acc_nw
  }

  # ── Step 10: Build groups_df ──────────────────────────────────────────────
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

  # ── Step 11: Build meta_args ──────────────────────────────────────────────
  group_meta <- .build_group_meta(design, group_vars)

  meta_args <- list(
    conf_level = conf_level,
    call = match.call(),
    group = group_meta,
    numerator = c(list(name = num_name), num_meta),
    denominator = c(list(name = denom_name), denom_meta)
  )

  # ── Step 12: Assemble result ───────────────────────────────────────────────
  result <- .make_result_tibble(
    col_vecs,
    groups_df,
    "survey_ratios",
    design,
    meta_args,
    RATIOS_META_KEYS
  )

  # ── Step 13: Apply decimals and name style ────────────────────────────────
  if (!is.null(decimals)) {
    result <- .apply_decimals(result, decimals)
  }
  .apply_name_style(result, name_style)
}
