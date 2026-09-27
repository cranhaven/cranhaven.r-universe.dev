# R/analysis-t-test.R
#
# Design-based two-sample t-test and all-pairs pairwise comparisons.
# Exported functions: get_t_test(), get_pairwise()
# Internal helpers: .enumerate_pairs()
# Shared helpers: analysis-helpers.R, analysis-diffs-helpers.R (.stars_pval)

# ── .enumerate_pairs() ────────────────────────────────────────────────────────

# Generate all C(k,2) pairs from a vector of factor levels.
# Returns a list of character(2) vectors: c(level_a, level_b), ordered by
# utils::combn() - all pairs starting with levels[1] first, then levels[2], etc.
#
# @param levels Character vector of k factor level strings (k >= 2).
# @return List of length C(k,2), each element a character(2) vector.
.enumerate_pairs <- function(levels) {
  utils::combn(levels, 2, simplify = FALSE)
}


# ── get_t_test() ──────────────────────────────────────────────────────────────

#' Design-Based Two-Sample T-Test for Survey Designs
#'
#' Compares the weighted means of two groups using a design-based t-test.
#' Follows the mathematical model of `survey::svyttest()` but uses
#' surveycore's own variance machinery (`survey_glm()`). Supports all four
#' survey design classes and optional subgroup analysis via `group`.
#'
#' @param design A survey design object: `survey_taylor`,
#'   `survey_replicate`, `survey_twophase`, or `survey_nonprob`.
#' @param x <[`tidy-select`][tidyselect::language]> A single unquoted
#'   numeric variable name for the outcome variable. Must resolve to exactly
#'   one numeric column.
#' @param by <[`tidy-select`][tidyselect::language]> A single unquoted
#'   variable name for the grouping variable. Must produce a model matrix
#'   with exactly 2 columns after fitting (intercept + one binary indicator).
#'   Character, integer, and logical columns are coerced to factor with a
#'   warning. Ordered factors are accepted as-is.
#' @param group <[`tidy-select`][tidyselect::language]> Optional subgroup
#'   variable(s). When supplied, the t-test is run separately within each
#'   unique combination of group values. Combined with any grouping set by
#'   `group_by()`. Default `NULL`.
#' @param conf_level Numeric(1). Confidence level strictly in (0, 1).
#'   Default `0.95`.
#' @param variance Character. Which uncertainty columns to include.
#'   Valid values: `"se"`, `"ci"`. Default `"ci"`. Both may be requested:
#'   `c("se", "ci")`.
#' @param na.rm Logical(1). Accepted for API uniformity with other
#'   `get_*()` functions. NA rows in `x` or `by` are always excluded
#'   (the GLM requires complete cases). Default `TRUE`.
#' @param min_cell_n Integer(1). Warn when either group has fewer than
#'   this many unweighted observations. Default `30L`. Use `0L` to
#'   suppress.
#' @param decimals Integer(1) or `NULL`. Round all double output columns
#'   to this many decimal places. `NULL` = no rounding. Default `NULL`.
#' @param label_values Logical(1). When `TRUE` (default), convert `by`
#'   and `group` factor codes to their value labels in the output.
#' @param label_vars Logical(1). Accepted for API uniformity; has no
#'   visible effect because column names are fixed. Default `TRUE`.
#' @param name_style Character(1). Output column naming style.
#'   `"surveycore"` (default) or `"broom"` (renames `se` to `std.error`,
#'   `ci_low` to `conf.low`, `ci_high` to `conf.high`, `p_value` to
#'   `p.value`, `df` to `parameter`). `t_stat` is not renamed.
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
#' @return A `survey_t_test` tibble (also inheriting `survey_result`).
#'   Columns: group columns (when active), `level_a`, `level_b`,
#'   `estimate`, `mean_a`, `mean_b`, `n_a`, `n_b`, `se` (optional),
#'   `ci_low` (optional), `ci_high` (optional), `t_stat`, `df`,
#'   `p_value`, `stars`. Use [meta()] to access design type, conf_level,
#'   and variable metadata.
#'
#' @examples
#' gss_sub <- gss_2024[gss_2024$sex %in% c(1L, 2L) & !is.na(gss_2024$age), ]
#' gss_sub$sex <- factor(
#'   gss_sub$sex,
#'   levels = c(1, 2),
#'   labels = c("Male", "Female")
#' )
#' gss_design <- as_survey(
#'   gss_sub,
#'   ids = vpsu,
#'   weights = wtssps,
#'   strata = vstrat,
#'   nest = TRUE
#' )
#' get_t_test(gss_design, age, by = sex)
#' @family analysis
#' @export
get_t_test <- function(
  design,
  x,
  by,
  group = NULL,
  conf_level = 0.95,
  variance = "ci",
  na.rm = TRUE,
  min_cell_n = 30L,
  decimals = NULL,
  label_values = TRUE,
  label_vars = TRUE,
  name_style = "surveycore",
  ...,
  .id = NULL,
  .if_missing_var = NULL
) {
  if (S7::S7_inherits(design, survey_collection)) {
    return(.dispatch_over_collection(
      get_t_test,
      design,
      x = {{ x }},
      by = {{ by }},
      group = {{ group }},
      ...,
      .id = .id,
      .if_missing_var = .if_missing_var
    ))
  }
  # Step 1: Validate design class
  .check_unsupported_class(design, "get_t_test")

  # Step 2: Validate shared args
  .validate_shared_args(
    variance,
    conf_level,
    name_style,
    decimals = decimals,
    na.rm = na.rm,
    valid_variance = c("se", "ci")
  )

  # Step 4: Resolve x
  x_name <- rlang::as_name(rlang::ensym(x))
  if (!x_name %in% names(design@data)) {
    # nocov start
    # Defensive: rlang::ensym() captures the bare column name; if it's not in
    # the data frame, R's NSE would have already failed at the call site.
    cli::cli_abort(
      c(
        "x" = "{.arg x} must select exactly one column.",
        "i" = "Column {.field {x_name}} not found in design data."
      ),
      class = "surveycore_error_wrong_variable_count"
    )
    # nocov end
  }

  # Step 5: Validate x is numeric
  x_col_check <- design@data[[x_name]]
  if (!is.numeric(x_col_check)) {
    cli::cli_abort(
      c(
        "x" = paste0(
          "{.arg x} must be numeric, not {.cls {class(x_col_check)[[1L]]}}. ",
          "Column {.field {x_name}} cannot be used with {.fn get_t_test}."
        )
      ),
      class = "surveycore_error_non_numeric_variable"
    )
  }

  # Step 6: Resolve by; coerce to factor (warn if needed)
  by_name <- rlang::as_name(rlang::ensym(by))
  if (!by_name %in% names(design@data)) {
    # nocov start
    # Defensive: same reasoning as the x-not-found branch above.
    cli::cli_abort(
      c(
        "x" = "{.arg by} must select exactly one column.",
        "i" = "Column {.field {by_name}} not found in design data."
      ),
      class = "surveycore_error_wrong_variable_count"
    )
    # nocov end
  }
  by_col <- design@data[[by_name]]
  if (!is.factor(by_col)) {
    if (is.ordered(by_col)) {
      # nocov start
      # Defensive: ordered() satisfies is.factor() so this branch is unreachable.
      by_col <- factor(as.character(by_col), levels = levels(by_col))
      # nocov end
    } else {
      # character, integer, logical: coerce and warn
      lvls <- sort(unique(by_col[!is.na(by_col)]))
      by_col <- factor(by_col, levels = lvls)
      cli::cli_warn(
        c(
          "!" = paste0(
            "{.field {by_name}} coerced to factor. ",
            "Level order: {.val {levels(by_col)}}."
          )
        ),
        class = "surveycore_warning_by_coerced"
      )
    }
    design@data[[by_name]] <- by_col
  }

  # Step 7: Resolve group variables
  group_quo <- rlang::enquo(group)
  group_vars <- .resolve_groups(design, group_quo)

  # Step 8: Build group strata
  domain_mask_full <- .apply_domain(design)
  if (length(group_vars) == 0L) {
    combos <- data.frame(row.names = 1L)
  } else {
    domain_data <- design@data[domain_mask_full, group_vars, drop = FALSE]
    combos <- .build_group_combos(domain_data, na.rm = TRUE)
  }

  # Capture the call for .meta
  cl <- match.call()

  # Step 9: For each group stratum
  all_rows <- vector("list", max(nrow(combos), 1L))
  meta_level_a <- NA_character_
  meta_level_b <- NA_character_

  for (i in seq_len(max(nrow(combos), 1L))) {
    # Build active_mask: group membership AND non-NA x AND non-NA by
    if (length(group_vars) == 0L) {
      group_mask <- domain_mask_full
      group_vals <- NULL
    } else {
      combo_row <- combos[i, , drop = FALSE]
      data_cols <- as.list(design@data[group_vars])
      group_match <- .match_group_combo(data_cols, combo_row)
      group_mask <- domain_mask_full & group_match
      group_vals <- as.character(unlist(combo_row))
    }
    active_mask <- group_mask &
      !is.na(design@data[[x_name]]) &
      !is.na(design@data[[by_name]])

    # Step 9b: Empty-cell check FIRST, then small-cell warning
    by_col_active <- design@data[[by_name]]
    # Compute active by-levels from group membership + non-NA by (not requiring
    # non-NA x). This ensures we detect empty cells from NA x rows too.
    by_group_mask <- group_mask & !is.na(design@data[[by_name]])
    active_levels_here <- levels(droplevels(by_col_active[by_group_mask]))

    for (lv in active_levels_here) {
      n_lv <- sum(by_col_active[active_mask] == lv, na.rm = TRUE)
      if (n_lv == 0L) {
        if (is.null(group_vals)) {
          cli::cli_abort(
            c(
              "x" = paste0(
                "Level {.field {lv}} of {.arg by} ",
                "({.field {by_name}}) has no non-NA observations. ",
                "Cannot compute t-test."
              )
            ),
            class = "surveycore_error_by_empty_cell"
          )
        } else {
          gv_str <- paste(
            group_vars,
            group_vals,
            sep = " = ",
            collapse = ", "
          )
          cli::cli_abort(
            c(
              "x" = paste0(
                "In group stratum {.val {gv_str}}, level {.field {lv}} ",
                "of {.arg by} ({.field {by_name}}) has no non-NA ",
                "observations."
              )
            ),
            class = "surveycore_error_by_empty_cell"
          )
        }
      }
    }
    # Small-cell warning (only if n > 0)
    for (lv in active_levels_here) {
      n_lv <- sum(by_col_active[active_mask] == lv, na.rm = TRUE)
      if (n_lv > 0L && n_lv < min_cell_n) {
        cli::cli_warn(
          c(
            "!" = paste0(
              "1 cell has fewer than {min_cell_n} unweighted observations. ",
              "Estimates in these cells may be unreliable for public ",
              "reporting (AAPOR guidance)."
            )
          ),
          class = "surveycore_warning_small_cell"
        )
      }
    }

    # Step 9c: Fit survey_glm with domain mask
    # Restrict by-factor to only the active levels (those present in domain
    # rows) so model.matrix produces exactly 2 columns. This is necessary when
    # the full data has more than 2 factor levels but the domain mask restricts
    # to 2 active levels — model.matrix does not drop levels automatically.
    design_g <- design
    design_g@data[[SURVEYCORE_DOMAIN_COL]] <- active_mask
    design_g@data[[by_name]] <- factor(
      design_g@data[[by_name]],
      levels = active_levels_here
    )

    fit <- rlang::inject(survey_glm(
      !!rlang::sym(x_name) ~ !!rlang::sym(by_name),
      design = design_g,
      family = stats::gaussian()
    ))

    # Step 9d: Verify model has exactly 2 coefficients
    n_coef <- length(stats::coef(fit))
    if (n_coef != 2L) {
      n_active_grps <- n_coef - 1L
      cli::cli_abort(
        c(
          "x" = paste0(
            "{.arg by} ({.field {by_name}}) must produce exactly 2 active ",
            "groups, not {n_active_grps}. Filter the design to 2 groups with ",
            "{.fn surveytidy::filter}, or use {.fn get_pairwise} for k > 2 ",
            "groups."
          )
        ),
        class = "surveycore_error_by_not_two_levels"
      )
    }

    # Step 9e: Extract statistics
    beta <- stats::coef(fit) # length 2: [intercept, slope]
    V <- fit@vcov # 2x2 design-based sandwich covariance

    se_val <- sqrt(V[2L, 2L])
    t_stat_val <- beta[2L] / se_val
    df_val <- max(1, fit@degf - 1L)
    p_value_val <- 2 * stats::pt(-abs(t_stat_val), df = df_val)

    ci_low_val <- beta[2L] -
      stats::qt((1 + conf_level) / 2, df = df_val) * se_val
    ci_high_val <- beta[2L] +
      stats::qt((1 + conf_level) / 2, df = df_val) * se_val

    # Step 9f: Group means and counts
    mean_a_val <- beta[1L]
    mean_b_val <- beta[1L] + beta[2L]

    # Active factor levels in order
    fac_levels <- levels(droplevels(design@data[[by_name]][active_mask]))
    level_a_val <- fac_levels[1L]
    level_b_val <- fac_levels[2L]

    n_a_val <- as.integer(sum(
      by_col_active[active_mask] == level_a_val,
      na.rm = TRUE
    ))
    n_b_val <- as.integer(sum(
      by_col_active[active_mask] == level_b_val,
      na.rm = TRUE
    ))

    # Step 9g: Handle label_values for level_a / level_b
    tmp_df <- data.frame(val = c(level_a_val, level_b_val))
    names(tmp_df) <- by_name
    converted_levels <- .apply_group_labels(
      tmp_df,
      by_name,
      design,
      label_values
    )[[1L]]
    level_a_out <- as.character(converted_levels[[1L]])
    level_b_out <- as.character(converted_levels[[2L]])

    # Assemble row
    row_list <- list(
      level_a = level_a_out,
      level_b = level_b_out,
      estimate = beta[2L],
      mean_a = mean_a_val,
      mean_b = mean_b_val,
      n_a = n_a_val,
      n_b = n_b_val
    )

    if ("se" %in% variance) {
      row_list$se <- se_val
    }
    if ("ci" %in% variance) {
      row_list$ci_low <- ci_low_val
      row_list$ci_high <- ci_high_val
    }

    row_list$t_stat <- t_stat_val
    row_list$df <- df_val
    row_list$p_value <- p_value_val
    row_list$stars <- .stars_pval(p_value_val)

    # Attach group columns if active
    if (length(group_vars) > 0L) {
      combo_row_clean <- combos[i, , drop = FALSE]
      rownames(combo_row_clean) <- NULL
      all_rows[[i]] <- cbind(
        combo_row_clean,
        as.data.frame(row_list, stringsAsFactors = FALSE)
      )
    } else {
      all_rows[[i]] <- as.data.frame(row_list, stringsAsFactors = FALSE)
    }
    # Store raw levels for .meta (use first stratum only)
    if (i == 1L) {
      meta_level_a <- level_a_val
      meta_level_b <- level_b_val
    }
  }

  # Step 10: Stack rows
  result <- do.call(rbind, all_rows)
  rownames(result) <- NULL

  # Step 11: Apply label_values to group columns
  if (length(group_vars) > 0L) {
    groups_df <- result[group_vars]
    groups_df <- .apply_group_labels(
      groups_df,
      group_vars,
      design,
      label_values
    )
    result[group_vars] <- groups_df
  }

  # Step 12: Convert to tibble
  result <- tibble::as_tibble(result)

  # Step 13: Apply decimals
  if (!is.null(decimals)) {
    for (j in seq_along(result)) {
      if (is.double(result[[j]])) result[[j]] <- round(result[[j]], decimals)
    }
  }

  # Step 14: Apply name_style
  result_class <- c(
    "survey_t_test",
    "survey_result",
    "tbl_df",
    "tbl",
    "data.frame"
  )
  class(result) <- result_class
  result <- .apply_name_style(result, name_style, exclude = NULL)

  # Attach column-level label attributes
  by_label <- design@metadata@variable_labels[[by_name]]
  if (is.null(by_label) || identical(by_label, "")) {
    by_label <- by_name
  }

  col_labels <- list(
    level_a = paste0(by_label, " (A)"),
    level_b = paste0(by_label, " (B)"),
    estimate = "Difference (B \u2212 A)",
    mean_a = "Mean (A)",
    mean_b = "Mean (B)",
    n_a = "N (A)",
    n_b = "N (B)",
    se = "Std. Error",
    std.error = "Std. Error",
    ci_low = "Low CI",
    conf.low = "Low CI",
    ci_high = "High CI",
    conf.high = "High CI",
    t_stat = "t",
    df = "df",
    parameter = "df",
    p_value = "P-Value",
    p.value = "P-Value",
    stars = ""
  )
  for (nm in names(result)) {
    if (nm %in% group_vars) {
      gl <- design@metadata@variable_labels[[nm]]
      attr(result[[nm]], "label") <- if (!is.null(gl)) gl else nm
    } else if (!is.null(col_labels[[nm]])) {
      attr(result[[nm]], "label") <- col_labels[[nm]]
    }
  }

  # Step 15: Assemble .meta
  by_meta <- .extract_var_meta(design, by_name)
  by_meta$levels <- c(meta_level_a, meta_level_b)

  meta_args <- list(
    conf_level = conf_level,
    call = cl,
    group = .build_group_meta(design, group_vars),
    x = stats::setNames(list(.extract_var_meta(design, x_name)), x_name),
    by = by_meta
  )

  attr(result, ".meta") <- .build_meta(design, meta_args)
  stopifnot(all(T_TEST_META_KEYS %in% names(meta_args)))
  class(result) <- result_class

  result
}


# ── .extract_print_label() ───────────────────────────────────────────────────
#
# Helper used by both print.survey_t_test and print.survey_pairwise.
# Returns the by-variable label (from .meta) when one is set and non-empty;
# otherwise falls back to the raw column name stored in the call.
#
# @param m The .meta list (attr(x, ".meta")).
# @return A character string for display in the "By:" header line.
.extract_print_label <- function(m) {
  lbl <- m$by$variable_label
  if (!is.null(lbl) && nzchar(lbl)) {
    lbl
  } else {
    deparse(m$call$by)
  }
}


# ── print.survey_t_test() ─────────────────────────────────────────────────────

#' Print method for survey_t_test objects.
#' @param x A `survey_t_test` object.
#' @param ... Additional arguments (unused).
#' @return `x`, invisibly.
#' @method print survey_t_test
#' @export
print.survey_t_test <- function(x, ...) {
  m <- attr(x, ".meta")
  design_label <- switch(
    m$design_type,
    taylor = "Taylor series",
    replicate = "Replicate weights",
    twophase = "Two-phase",
    nonprob = "Non-probability",
    m$design_type
  )
  n_fmt <- format(m$n_respondents, big.mark = ",")

  x_name <- names(m$x)[[1L]]
  x_label <- m$x[[x_name]]$variable_label
  if (is.null(x_label) || identical(x_label, "")) {
    x_label <- x_name
  }

  by_label <- .extract_print_label(m)

  level_a_disp <- if (nrow(x) > 0L) x[["level_a"]][[1L]] else m$by$levels[[1L]]
  level_b_disp <- if (nrow(x) > 0L) x[["level_b"]][[1L]] else m$by$levels[[2L]]

  cat("# A survey_t_test result\n")
  cat(sprintf("# Design: %s | N: %s\n", design_label, n_fmt))
  cat(sprintf(
    "# DV: %s | By: %s (%s vs. %s)\n",
    x_label,
    by_label,
    level_a_disp,
    level_b_disp
  ))

  class(x) <- setdiff(class(x), c("survey_t_test", "survey_result"))
  NextMethod()
  invisible(x)
}


# ── get_pairwise() ────────────────────────────────────────────────────────────

#' All-Pairs Pairwise T-Tests for Survey Designs
#'
#' Runs all k(k-1)/2 pairwise two-sample t-tests for a grouping variable
#' with k levels and applies multiple-comparison p-value adjustment.
#' Delegates pair-level computations to [get_t_test()].
#'
#' @param design A survey design object: `survey_taylor`,
#'   `survey_replicate`, `survey_twophase`, or `survey_nonprob`.
#' @param x <[`tidy-select`][tidyselect::language]> A single unquoted
#'   numeric variable name for the outcome variable.
#' @param by <[`tidy-select`][tidyselect::language]> A single unquoted
#'   variable name for the grouping variable. Must have at least 2 active
#'   levels.
#' @param group <[`tidy-select`][tidyselect::language]> Optional subgroup
#'   variable(s). When supplied, pairwise comparisons are run within each
#'   group stratum. P-value adjustment is applied separately per stratum.
#'   Default `NULL`.
#' @param pval_adj Character(1). P-value adjustment method passed to
#'   [stats::p.adjust()]. Default `"holm"`. Use `"none"` for unadjusted
#'   p-values. Error: `surveycore_error_invalid_pval_adj`.
#' @param conf_level Numeric(1). Confidence level strictly in (0, 1).
#'   Default `0.95`.
#' @param variance Character. Which uncertainty columns to include.
#'   Valid values: `"se"`, `"ci"`. Default `"ci"`.
#' @param na.rm Logical(1). Accepted for API uniformity. Default `TRUE`.
#' @param min_cell_n Integer(1). Warn for small cells. Default `30L`.
#' @param decimals Integer(1) or `NULL`. Round all double output columns.
#'   Default `NULL`.
#' @param label_values Logical(1). Convert `by`/`group` codes to value
#'   labels. Default `TRUE`.
#' @param label_vars Logical(1). Accepted for API uniformity; no visible
#'   effect. Default `TRUE`.
#' @param name_style Character(1). `"surveycore"` (default) or `"broom"`.
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
#' @return A `survey_pairwise` tibble (also inheriting `survey_result`).
#'   Columns: group columns (when active), `level_a`, `level_b`,
#'   `estimate`, `mean_a`, `mean_b`, `n_a`, `n_b`, `se` (optional),
#'   `ci_low` (optional), `ci_high` (optional), `t_stat`, `df`,
#'   `p_value` (adjusted), `stars`. Use [meta()] to access the
#'   adjustment method and other metadata.
#'
#' @examples
#' gss_sub <- gss_2024[gss_2024$sex %in% c(1L, 2L) & !is.na(gss_2024$age), ]
#' gss_sub$sex <- factor(
#'   gss_sub$sex,
#'   levels = c(1, 2),
#'   labels = c("Male", "Female")
#' )
#' gss_design <- as_survey(
#'   gss_sub,
#'   ids = vpsu,
#'   weights = wtssps,
#'   strata = vstrat,
#'   nest = TRUE
#' )
#' get_pairwise(gss_design, age, by = sex)
#' @family analysis
#' @export
get_pairwise <- function(
  design,
  x,
  by,
  group = NULL,
  pval_adj = "holm",
  conf_level = 0.95,
  variance = "ci",
  na.rm = TRUE,
  min_cell_n = 30L,
  decimals = NULL,
  label_values = TRUE,
  label_vars = TRUE,
  name_style = "surveycore",
  ...,
  .id = NULL,
  .if_missing_var = NULL
) {
  if (S7::S7_inherits(design, survey_collection)) {
    return(.dispatch_over_collection(
      get_pairwise,
      design,
      x = {{ x }},
      by = {{ by }},
      group = {{ group }},
      ...,
      .id = .id,
      .if_missing_var = .if_missing_var
    ))
  }
  # Step 1: Validate design class
  .check_unsupported_class(design, "get_pairwise")

  # Step 2: Validate shared args
  .validate_shared_args(
    variance,
    conf_level,
    name_style,
    decimals = decimals,
    na.rm = na.rm,
    valid_variance = c("se", "ci")
  )

  # Step 3: Validate pval_adj
  valid_methods <- stats::p.adjust.methods
  if (!pval_adj %in% valid_methods) {
    cli::cli_abort(
      c(
        "x" = paste0(
          "{.arg pval_adj} must be a valid method for ",
          "{.fn stats::p.adjust}."
        ),
        "i" = "Valid methods: {.or {.val {valid_methods}}}.",
        "i" = "Got {.val {pval_adj}}."
      ),
      class = "surveycore_error_invalid_pval_adj"
    )
  }

  # Step 4: Resolve x
  x_name <- rlang::as_name(rlang::ensym(x))
  if (!x_name %in% names(design@data)) {
    # nocov start
    # Defensive: same reasoning as the x-not-found branch in get_t_test().
    cli::cli_abort(
      c(
        "x" = "{.arg x} must select exactly one column.",
        "i" = "Column {.field {x_name}} not found in design data."
      ),
      class = "surveycore_error_wrong_variable_count"
    )
    # nocov end
  }

  # 4a: Compute domain mask once before pair loop
  domain_mask <- .apply_domain(design)

  # Step 5: Resolve by; coerce to factor; compute active levels
  by_name <- rlang::as_name(rlang::ensym(by))
  if (!by_name %in% names(design@data)) {
    # nocov start
    # Defensive: same reasoning as the by-not-found branch in get_t_test().
    cli::cli_abort(
      c(
        "x" = "{.arg by} must select exactly one column.",
        "i" = "Column {.field {by_name}} not found in design data."
      ),
      class = "surveycore_error_wrong_variable_count"
    )
    # nocov end
  }
  by_col <- design@data[[by_name]]
  if (!is.factor(by_col)) {
    if (is.ordered(by_col)) {
      # nocov start
      # Defensive: ordered() satisfies is.factor() so this branch is unreachable.
      by_col <- factor(as.character(by_col), levels = levels(by_col))
      # nocov end
    } else {
      lvls <- sort(unique(by_col[!is.na(by_col)]))
      by_col <- factor(by_col, levels = lvls)
      cli::cli_warn(
        c(
          "!" = paste0(
            "{.field {by_name}} coerced to factor. ",
            "Level order: {.val {levels(by_col)}}."
          )
        ),
        class = "surveycore_warning_by_coerced"
      )
    }
    design@data[[by_name]] <- by_col
  }

  # Active levels: those with >= 1 non-NA observation after domain mask.
  # Use as.integer() + tabulate with explicit nbins to match nlevels(by_col),
  # avoiding the recycling trap when the max code in the domain subset is
  # smaller than nlevels (i.e., some levels are absent from domain rows).
  by_int_domain <- as.integer(by_col[domain_mask])
  counts_per_level <- tabulate(by_int_domain, nbins = nlevels(by_col))
  active_levels <- levels(by_col)[counts_per_level > 0L]
  n_active <- length(active_levels)
  if (n_active < 2L) {
    cli::cli_abort(
      c(
        "x" = paste0(
          "{.arg by} ({.field {by_name}}) must have at least 2 active ",
          "groups, not {n_active}."
        )
      ),
      class = "surveycore_error_by_one_level"
    )
  }

  # Step 6: Capture group quosure for forwarding
  group_quo <- rlang::enquo(group)

  # Step 7: Enumerate pairs
  pairs <- .enumerate_pairs(active_levels)

  # Step 8: For each pair, run get_t_test()
  all_pair_results <- vector("list", length(pairs))

  for (pi in seq_along(pairs)) {
    pair <- pairs[[pi]]
    a <- pair[1L]
    b <- pair[2L]

    # Domain-column mutation: restrict to rows in this pair
    design_ab <- design
    design_ab@data[[SURVEYCORE_DOMAIN_COL]] <-
      by_col %in% c(a, b) & domain_mask

    # Re-level so a is the reference
    design_ab@data[[by_name]] <- factor(
      design_ab@data[[by_name]],
      levels = c(a, b)
    )

    # Forward to get_t_test() via rlang inject
    pair_result <- rlang::inject(get_t_test(
      design_ab,
      x = !!rlang::sym(x_name),
      by = !!rlang::sym(by_name),
      group = !!group_quo,
      conf_level = conf_level,
      variance = variance,
      na.rm = na.rm,
      min_cell_n = min_cell_n,
      decimals = NULL,
      label_values = FALSE,
      label_vars = label_vars,
      name_style = "surveycore"
    ))

    # Overwrite level_a/level_b with raw pair values (prevent double-conversion)
    pair_result$level_a <- a
    pair_result$level_b <- b

    all_pair_results[[pi]] <- pair_result
  }

  # Step 9: Stack pair results
  result <- do.call(rbind, lapply(all_pair_results, as.data.frame))
  rownames(result) <- NULL
  result <- tibble::as_tibble(result)

  # Step 10: Apply label_values to level_a/level_b (once, post-stacking)
  if (label_values) {
    all_vals <- unique(c(result$level_a, result$level_b))
    tmp_df <- data.frame(val = all_vals, stringsAsFactors = FALSE)
    names(tmp_df) <- by_name
    converted <- .apply_group_labels(
      tmp_df,
      by_name,
      design,
      label_values
    )[[1L]]
    lookup <- stats::setNames(as.character(converted), all_vals)
    result$level_a <- unname(lookup[result$level_a])
    result$level_b <- unname(lookup[result$level_b])
  }

  # Identify group columns for p-value adjustment
  group_vars <- .resolve_groups(design, group_quo)

  # Step 11: Apply p-value adjustment
  if (length(group_vars) == 0L) {
    result$p_value <- stats::p.adjust(result$p_value, method = pval_adj)
  } else {
    # Apply separately within each group stratum
    group_combos_stacked <- unique(result[group_vars])
    for (gi in seq_len(nrow(group_combos_stacked))) {
      combo <- group_combos_stacked[gi, , drop = FALSE]
      row_mask <- rep(TRUE, nrow(result))
      for (gv in group_vars) {
        row_mask <- row_mask & (result[[gv]] == combo[[gv]])
      }
      result$p_value[row_mask] <- stats::p.adjust(
        result$p_value[row_mask],
        method = pval_adj
      )
    }
  }

  # Step 12: Recompute stars from adjusted p-values
  result$stars <- vapply(result$p_value, .stars_pval, character(1L))

  # Step 13: Apply decimals
  if (!is.null(decimals)) {
    for (j in seq_along(result)) {
      if (is.double(result[[j]])) result[[j]] <- round(result[[j]], decimals)
    }
  }

  # Step 14: Apply name_style and column-level labels
  result_class <- c(
    "survey_pairwise",
    "survey_result",
    "tbl_df",
    "tbl",
    "data.frame"
  )
  class(result) <- result_class
  result <- .apply_name_style(result, name_style, exclude = NULL)

  by_label <- design@metadata@variable_labels[[by_name]]
  if (is.null(by_label) || identical(by_label, "")) {
    by_label <- by_name
  }

  col_labels <- list(
    level_a = paste0(by_label, " (A)"),
    level_b = paste0(by_label, " (B)"),
    estimate = "Difference (B \u2212 A)",
    mean_a = "Mean (A)",
    mean_b = "Mean (B)",
    n_a = "N (A)",
    n_b = "N (B)",
    se = "Std. Error",
    std.error = "Std. Error",
    ci_low = "Low CI",
    conf.low = "Low CI",
    ci_high = "High CI",
    conf.high = "High CI",
    t_stat = "t",
    df = "df",
    parameter = "df",
    p_value = paste0("P-Value (", pval_adj, ")"),
    p.value = paste0("P-Value (", pval_adj, ")"),
    stars = ""
  )
  for (nm in names(result)) {
    if (nm %in% group_vars) {
      gl <- design@metadata@variable_labels[[nm]]
      attr(result[[nm]], "label") <- if (!is.null(gl)) gl else nm
    } else if (!is.null(col_labels[[nm]])) {
      attr(result[[nm]], "label") <- col_labels[[nm]]
    }
  }

  # Step 15: Assemble .meta
  by_meta <- .extract_var_meta(design, by_name)
  by_meta$levels <- active_levels

  meta_args <- list(
    conf_level = conf_level,
    call = match.call(),
    group = .build_group_meta(design, group_vars),
    x = stats::setNames(list(.extract_var_meta(design, x_name)), x_name),
    by = by_meta,
    pval_adj = pval_adj
  )

  attr(result, ".meta") <- .build_meta(design, meta_args)
  stopifnot(all(PAIRWISE_META_KEYS %in% names(meta_args)))
  class(result) <- result_class

  result
}


# ── print.survey_pairwise() ───────────────────────────────────────────────────

#' Print method for survey_pairwise objects.
#' @param x A `survey_pairwise` object.
#' @param ... Additional arguments (unused).
#' @return `x`, invisibly.
#' @method print survey_pairwise
#' @export
print.survey_pairwise <- function(x, ...) {
  m <- attr(x, ".meta")
  design_label <- switch(
    m$design_type,
    taylor = "Taylor series",
    replicate = "Replicate weights",
    twophase = "Two-phase",
    nonprob = "Non-probability",
    m$design_type
  )
  n_fmt <- format(m$n_respondents, big.mark = ",")

  x_name <- names(m$x)[[1L]]
  x_label <- m$x[[x_name]]$variable_label
  if (is.null(x_label) || identical(x_label, "")) {
    x_label <- x_name
  }

  by_label <- .extract_print_label(m)

  k <- length(m$by$levels)
  n_pairs <- k * (k - 1L) / 2L

  cat("# A survey_pairwise result\n")
  cat(sprintf("# Design: %s | N: %s\n", design_label, n_fmt))
  cat(sprintf(
    "# DV: %s | By: %s (%d levels, %d pairs)\n",
    x_label,
    by_label,
    k,
    n_pairs
  ))
  cat(sprintf("# Adjustment: %s\n", m$pval_adj))

  class(x) <- setdiff(class(x), c("survey_pairwise", "survey_result"))
  NextMethod()
  invisible(x)
}
