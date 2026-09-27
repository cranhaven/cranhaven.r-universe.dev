# R/analysis-totals.R
#
# Weighted total estimation for survey designs.
# Exported function: get_totals()
#
# Internal helpers: analysis-totals-helpers.R

# ── get_totals() ──────────────────────────────────────────────────────────────

#' Weighted Total for a Survey Design
#'
#' Compute the estimated population total of a numeric variable in a survey
#' design, or the estimated population size when no variable is supplied.
#' Supports optional grouping, uncertainty quantification, and metadata-driven
#' labelling.
#'
#' @param design A survey design object: `survey_taylor`, `survey_replicate`,
#'   `survey_twophase`, or `survey_nonprob`.
#' @param x <[`tidy-select`][tidyselect::language]> Optional single unquoted
#'   numeric variable name. When `NULL` (default), estimates the population
#'   size (sum of weights). When supplied, estimates the weighted sum
#'   (sum of w_i * x_i).
#' @param group <[`tidy-select`][tidyselect::language]> Optional grouping
#'   variable(s). Default `NULL`.
#' @param variance `NULL` or a character vector from `"se"`, `"ci"`, `"var"`,
#'   `"cv"`, `"moe"`, `"deff"`. Default `"ci"`.
#' @param conf_level Numeric scalar in (0, 1). Default `0.95`.
#' @param n_weighted Logical. For `get_totals(d)` (no variable), equals the
#'   `total` column and is included for API uniformity. For variable mode,
#'   adds the sum of weights for non-NA observations. Default `FALSE`.
#' @param decimals Integer or `NULL`. If an integer, rounds all numeric output
#'   columns (e.g., `total`, `se`, `ci_low`, `ci_high`) to this many decimal
#'   places. Default `NULL` (no rounding).
#' @param min_cell_n Integer. Default `30L`.
#' @param na.rm Logical. If `TRUE` (default), `NA` values are excluded from
#'   analysis: observations where the analysis variable is `NA` are dropped
#'   from calculations, and observations where any group variable is `NA` are
#'   excluded from the output. If `FALSE`, `NA` observations in the analysis
#'   variable are included in calculations, and observations where a group
#'   variable is `NA` are collected into their own group row in the output
#'   (appearing after all non-`NA` group rows).
#' @param label_values Logical. Accepted for API consistency across `get_*()`
#'   functions. For `get_totals()`, no value-level cells appear in the output,
#'   so this parameter has no effect. Default `TRUE`.
#' @param label_vars Logical. Accepted for API uniformity. Default `TRUE`.
#' @param name_style `"surveycore"` (default) or `"broom"`.
#' @param ... Additional arguments forwarded to `.dispatch_over_collection()`
#'   when `design` is a [`survey_collection`]. For single-survey inputs these
#'   arguments are ignored.
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
#' @return A `survey_totals` tibble (also inheriting `survey_result`). Columns:
#' \itemize{
#'   \item `[group_cols...]` — group variable columns (when active), first.
#'   \item `total` — the weighted sum estimate.
#'   \item Variance columns — only those requested via `variance`.
#'   \item `n` — unweighted count (omitted in no-variable mode).
#'   \item `n_weighted` — sum of weights (only when requested).
#' }
#' The variable name (or `NULL` for no-variable mode) is in
#' `meta(result)$x`. Use `meta(result)` for additional metadata.
#'
#' @examples
#' d <- as_survey_replicate(
#'   acs_pums_wy,
#'   weights = pwgtp,
#'   repweights = pwgtp1:pwgtp80,
#'   type = "successive-difference"
#' )
#'
#' # Population size
#' get_totals(d)
#'
#' # Total for a variable
#' get_totals(d, agep)
#'
#' # Grouped
#' get_totals(d, agep, group = sex)
#' @family analysis
#' @export
get_totals <- function(
  design,
  x = NULL,
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
      get_totals,
      design,
      x = {{ x }},
      group = {{ group }},
      ...,
      .id = .id,
      .if_missing_var = .if_missing_var
    ))
  }
  # ── Step 1: Validate ────────────────────────────────────────────────────────
  .check_unsupported_class(design, "get_totals")
  .validate_shared_args(
    variance,
    conf_level,
    name_style,
    decimals = decimals,
    na.rm = na.rm
  )

  # ── Step 2: Resolve variable, groups, domain ────────────────────────────────
  x_quo <- rlang::enquo(x)
  group_quo <- rlang::enquo(group)

  no_variable <- rlang::quo_is_null(x_quo)

  if (no_variable) {
    x_name <- NULL
    x_col <- NULL
  } else {
    x_names <- .resolve_tidy_select(x_quo, design@data)

    if (length(x_names) != 1L) {
      cli::cli_abort(
        c(
          "x" = "{.fn get_totals} requires exactly one variable.",
          "i" = "{.arg x} resolved to {length(x_names)} variable{?s}."
        ),
        class = "surveycore_error_wrong_variable_count"
      )
    }

    x_name <- x_names[[1L]]
    x_col <- design@data[[x_name]]

    if (!is.numeric(x_col)) {
      cli::cli_abort(
        c(
          "x" = "{.arg x} must be numeric, not {.cls {class(x_col)}}.",
          "i" = "Column {.field {x_name}} cannot be used with {.fn get_totals}."
        ),
        class = "surveycore_error_non_numeric_variable"
      )
    }
  }

  group_vars <- .resolve_groups(design, group_quo)
  domain_mask <- .apply_domain(design)
  degf <- Inf # Normal approximation; matches survey::svytotal() default

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

  # ── Step 5: Collect variable metadata ───────────────────────────────────────
  x_meta <- if (!no_variable) .extract_var_meta(design, x_name) else NULL

  # ── Step 6: Main accumulation loop ──────────────────────────────────────────
  acc_total <- numeric(0)
  acc_se <- numeric(0)
  acc_sesrs <- numeric(0)
  acc_n <- integer(0)
  acc_nw <- numeric(0)
  acc_grp_rows <- vector("list", 0L)

  small_cell_ns <- integer(0)

  for (ci in seq_len(n_combos)) {
    if (length(group_vars) > 0L) {
      combo_row <- group_combos[ci, , drop = FALSE]
      data_cols <- as.list(design@data[group_vars])
      group_match <- .match_group_combo(data_cols, combo_row)
      active_mask <- domain_mask & group_match
    } else {
      active_mask <- domain_mask
    }

    if (no_variable) {
      # Population size: domain includes all in-mask rows (no NA filtering)
      domain <- as.numeric(active_mask)
    } else {
      domain <- .mean_domain_vec(active_mask, x_col, na.rm)
    }

    cell <- .total_cell(design, x_name, domain)

    if (!no_variable && !is.na(cell$n) && cell$n > 0L && cell$n < min_cell_n) {
      small_cell_ns <- c(small_cell_ns, cell$n)
    }

    acc_total <- c(acc_total, cell$total)
    acc_se <- c(acc_se, cell$se)
    acc_sesrs <- c(acc_sesrs, cell$se_srs)
    acc_nw <- c(acc_nw, cell$n_weighted)

    if (!no_variable) {
      acc_n <- c(acc_n, cell$n)
    }

    if (length(group_vars) > 0L) {
      acc_grp_rows <- c(acc_grp_rows, list(combo_row))
    }
  }

  # ── Step 7: Small-cell warning ──────────────────────────────────────────────
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

  # ── Step 8: Build column vectors ────────────────────────────────────────────
  var_cols <- .add_variance_cols(
    se_vec = acc_se,
    estimate_vec = acc_total,
    se_srs_vec = acc_sesrs,
    conf_level = conf_level,
    degf = degf,
    variance = variance
  )

  col_vecs <- list()
  col_vecs$total <- acc_total
  col_vecs <- c(col_vecs, var_cols)

  if (!no_variable) {
    col_vecs$n <- acc_n
  }

  if (isTRUE(n_weighted)) {
    col_vecs$n_weighted <- acc_nw
  }

  # ── Step 9: Build groups_df ─────────────────────────────────────────────────
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

  # ── Step 10: Build meta_args ────────────────────────────────────────────────
  group_meta <- .build_group_meta(design, group_vars)
  x_list <- if (is.null(x_meta)) {
    NULL
  } else {
    stats::setNames(list(x_meta), x_name)
  }

  meta_args <- list(
    conf_level = conf_level,
    call = match.call(),
    group = group_meta,
    x = x_list
  )

  # ── Step 11: Assemble result ────────────────────────────────────────────────
  result <- .make_result_tibble(
    col_vecs,
    groups_df,
    "survey_totals",
    design,
    meta_args,
    TOTALS_META_KEYS
  )

  # ── Step 12: Apply decimals and name style ──────────────────────────────────
  if (!is.null(decimals)) {
    result <- .apply_decimals(result, decimals)
  }
  .apply_name_style(result, name_style)
}
