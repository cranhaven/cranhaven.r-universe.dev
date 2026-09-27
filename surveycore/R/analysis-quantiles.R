# R/analysis-quantiles.R
#
# Survey-weighted quantile estimation (Woodruff 1952 method).
# Exported function: get_quantiles()
#
# Internal helpers: analysis-quantiles-helpers.R

# ── get_quantiles() ───────────────────────────────────────────────────────────

#' Survey-Weighted Quantiles
#'
#' Compute survey-weighted quantiles (including the median) for a single
#' numeric variable using the Woodruff (1952) confidence interval method.
#' Supports optional grouping, domain estimation, and all five survey design
#' classes.
#'
#' @param design A survey design object: `survey_taylor`, `survey_replicate`,
#'   `survey_twophase`, or `survey_nonprob`.
#' @param x <[`tidy-select`][tidyselect::language]> A single unquoted numeric
#'   variable name. Must resolve to exactly one numeric column.
#' @param probs Numeric vector of probabilities in (0, 1). Default
#'   `c(0.25, 0.5, 0.75)` (IQR + median).
#' @param group <[`tidy-select`][tidyselect::language]> Optional grouping
#'   variable(s). Combined with any grouping set by `group_by()`. Default
#'   `NULL`.
#' @param variance `NULL` or a character vector from `"se"`, `"ci"`, `"var"`,
#'   `"cv"`, `"moe"`, `"deff"`. Controls which uncertainty columns appear in
#'   the output. CIs use the Woodruff (1952) back-transformation method and
#'   are **not** symmetric around the estimate. `"deff"` is always `NA` for
#'   quantiles (no closed-form SRS SE). Default `"ci"`.
#' @param conf_level Numeric scalar in (0, 1). Confidence level for Woodruff
#'   intervals. Default `0.95`.
#' @param n_weighted Logical. If `TRUE`, add an `n_weighted` column with the
#'   sum of weights for non-NA observations in each group. Default `FALSE`.
#' @param decimals Integer or `NULL`. If an integer, rounds all numeric output
#'   columns (e.g., `estimate`, `se`, `ci_low`, `ci_high`) to this many
#'   decimal places. Default `NULL` (no rounding).
#' @param min_cell_n Integer. Minimum unweighted cell count before
#'   `surveycore_warning_small_cell` fires. Default `30L` (AAPOR guidance).
#' @param na.rm Logical. If `TRUE` (default), `NA` values in the analysis
#'   variable are excluded from calculations. If `FALSE`, any `NA` values in
#'   the analysis variable cause all quantile estimates for that cell to be
#'   `NA_real_`. Observations where any group variable is `NA` are always
#'   excluded from the output when `na.rm = TRUE`; when `na.rm = FALSE` they
#'   are collected into their own group row (appearing after all non-`NA` rows).
#' @param label_values Logical. Accepted for API consistency across `get_*()`
#'   functions. For `get_quantiles()`, no value-level cells appear in the
#'   output, so this parameter has no effect. Default `TRUE`.
#' @param label_vars Logical. Accepted for API uniformity; has no visible
#'   effect on `get_quantiles()` output. Default `TRUE`.
#' @param name_style `"surveycore"` (default) or `"broom"`. When `"broom"`,
#'   renames `se` → `std.error`, `ci_low` → `conf.low`,
#'   `ci_high` → `conf.high`. The `estimate` column is unchanged.
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
#' @return A `survey_quantiles` tibble (also inheriting `survey_result`).
#' \itemize{
#'   \item `[group_cols...]` — group variable columns (when active), first.
#'   \item `quantile` — probability label: `"p25"`, `"p50"`, etc.
#'   \item `estimate` — weighted quantile estimate.
#'   \item Variance columns (`se`, `var`, `cv`, `ci_low`, `ci_high`, `moe`,
#'     `deff`) — only those requested via `variance`. CIs are Woodruff
#'     intervals and are generally asymmetric around `estimate`. `deff` is
#'     always `NA` for quantile estimates: computing it requires a kernel
#'     density estimate at the quantile point (the Woodruff SRS approximation
#'     used by `survey::svyquantile(deff = TRUE)`), which is not implemented.
#'   \item `n` — unweighted count of observations in the active domain used
#'     in the estimate. When `na.rm = TRUE`, counts only non-NA observations;
#'     when `na.rm = FALSE`, counts all active-domain rows (including NAs,
#'     though the estimate will be `NA_real_`).
#'   \item `n_weighted` — sum of weights (only when requested).
#' }
#' One row per (group combination × quantile probability). The variable name
#' and `probs` vector are stored in `meta(result)`.
#'
#' @references
#' Woodruff, R. S. (1952). Confidence intervals for medians and other
#' position measures. *Journal of the American Statistical Association*,
#' **47**(260), 635–646.
#'
#' @examples
#' d <- as_survey(
#'   nhanes_2017,
#'   ids = sdmvpsu,
#'   weights = wtint2yr,
#'   strata = sdmvstra,
#'   nest = TRUE
#' )
#'
#' # IQR + median (default)
#' get_quantiles(d, ridageyr)
#'
#' # Median only with SE
#' get_quantiles(d, ridageyr, probs = 0.5, variance = c("ci", "se"))
#'
#' # Grouped quartiles
#' get_quantiles(d, ridageyr, group = riagendr)
#' @family analysis
#' @export
get_quantiles <- function(
  design,
  x,
  probs = c(0.25, 0.5, 0.75),
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
      get_quantiles,
      design,
      x = {{ x }},
      group = {{ group }},
      ...,
      .id = .id,
      .if_missing_var = .if_missing_var
    ))
  }
  # ── Step 1: Validate ────────────────────────────────────────────────────────
  .check_unsupported_class(design, "get_quantiles")
  .validate_shared_args(
    variance,
    conf_level,
    name_style,
    decimals = decimals,
    na.rm = na.rm
  )

  if (
    !is.numeric(probs) ||
      length(probs) == 0L ||
      any(is.na(probs)) ||
      any(probs <= 0 | probs >= 1)
  ) {
    bad_probs <- probs[is.na(probs) | probs <= 0 | probs >= 1]
    cli::cli_abort(
      c(
        "x" = paste0(
          "{.arg probs} must be a non-empty numeric vector with all values ",
          "in (0, 1)."
        ),
        "i" = "Invalid value{?s}: {.val {bad_probs}}."
      ),
      class = "surveycore_error_invalid_probs"
    )
  }

  # ── Step 2: Resolve variable, groups, domain ────────────────────────────────
  x_quo <- rlang::enquo(x)
  group_quo <- rlang::enquo(group)

  x_names <- .resolve_tidy_select(x_quo, design@data)

  if (length(x_names) != 1L) {
    cli::cli_abort(
      c(
        "x" = "{.fn get_quantiles} requires exactly one variable.",
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
        "i" = paste0(
          "Column {.field {x_name}} cannot be used ",
          "with {.fn get_quantiles}."
        )
      ),
      class = "surveycore_error_non_numeric_variable"
    )
  }

  group_vars <- .resolve_groups(design, group_quo)
  domain_mask <- .apply_domain(design)
  degf_w <- .degf_woodruff(design)

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
              " Grouped estimates will have a single row per quantile."
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
  x_meta <- .extract_var_meta(design, x_name)

  # Probability labels: 0.25 → "p25", 0.5 → "p50", etc.
  prob_labels <- paste0("p", round(probs * 100))

  # ── Step 6: Main accumulation loop ──────────────────────────────────────────
  acc_quantile <- character(0)
  acc_estimate <- numeric(0)
  acc_se <- numeric(0)
  acc_ci_low <- numeric(0)
  acc_ci_high <- numeric(0)
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

    # Build active_domain: exclude NAs for quantile computation
    has_na_in_domain <- any(is.na(x_col[active_mask]))
    active_domain <- as.numeric(active_mask & !is.na(x_col))
    n_d_combo <- as.integer(sum(active_domain > 0))

    # Small cell check: once per combo (n is the same across all probs)
    if (n_d_combo > 0L && n_d_combo < min_cell_n) {
      small_cell_ns <- c(small_cell_ns, n_d_combo)
    }

    for (pi in seq_along(probs)) {
      prob <- probs[[pi]]

      # na.rm = FALSE with NAs: the cell is undefined
      if (!na.rm && has_na_in_domain) {
        n_in_mask <- as.integer(sum(active_mask))
        cell <- list(
          estimate = NA_real_,
          se = NA_real_,
          ci_low = NA_real_,
          ci_high = NA_real_,
          se_srs = NA_real_,
          n = n_in_mask,
          n_weighted = NA_real_
        )
      } else {
        cell <- .quantile_woodruff_cell(
          design,
          x_col,
          active_domain,
          prob,
          conf_level,
          degf_w
        )
      }

      acc_quantile <- c(acc_quantile, prob_labels[[pi]])
      acc_estimate <- c(acc_estimate, cell$estimate)
      acc_se <- c(acc_se, cell$se)
      acc_ci_low <- c(acc_ci_low, cell$ci_low)
      acc_ci_high <- c(acc_ci_high, cell$ci_high)
      acc_n <- c(acc_n, cell$n)
      acc_nw <- c(acc_nw, cell$n_weighted)

      if (length(group_vars) > 0L) {
        acc_grp_rows <- c(acc_grp_rows, list(combo_row))
      }
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

  # ── Step 8: Build variance column vectors ───────────────────────────────────
  # Quantile CIs use Woodruff back-transformation (asymmetric); SE is derived
  # from the Woodruff CI. Cannot use .add_variance_cols() directly.
  var_cols <- list()

  if (!is.null(variance)) {
    if ("se" %in% variance) {
      var_cols$se <- acc_se
    }
    if ("var" %in% variance) {
      var_cols$var <- acc_se^2
    }
    if ("cv" %in% variance) {
      cv <- acc_se / acc_estimate
      is_undef <- !is.na(acc_estimate) & acc_estimate <= 0
      n_undef <- sum(is_undef)
      if (n_undef > 0L) {
        cv[is_undef] <- NA_real_
        cli::cli_warn(
          c(
            "!" = paste0(
              '{.arg variance = "cv"} is undefined for {n_undef} ',
              "cell{?s} where the estimate is 0 or negative."
            ),
            "i" = "{.code cv} set to {.code NA} for those cells."
          ),
          class = "surveycore_warning_cv_undefined"
        )
      }
      var_cols$cv <- cv
    }
    if ("ci" %in% variance) {
      var_cols$ci_low <- acc_ci_low
      var_cols$ci_high <- acc_ci_high
    }
    if ("moe" %in% variance) {
      var_cols$moe <- (acc_ci_high - acc_ci_low) / 2
    }
    if ("deff" %in% variance) {
      # DEFF requires se_srs, which for quantiles needs a kernel density
      # estimate at the quantile point (Woodruff SRS approximation).
      # Not implemented — deff is intentionally NA. See PR 8 in
      # plans/audit-remediation-plan.md for the implementation path.
      var_cols$deff <- rep(NA_real_, length(acc_estimate))
    }
  }

  # ── Step 9: Assemble col_vecs ───────────────────────────────────────────────
  col_vecs <- list()
  col_vecs$quantile <- acc_quantile
  col_vecs$estimate <- acc_estimate
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
  x_list <- stats::setNames(list(x_meta), x_name)

  meta_args <- list(
    conf_level = conf_level,
    call = match.call(),
    probs = probs,
    group = group_meta,
    x = x_list
  )

  # ── Step 12: Assemble result ────────────────────────────────────────────────
  result <- .make_result_tibble(
    col_vecs,
    groups_df,
    "survey_quantiles",
    design,
    meta_args,
    QUANTILES_META_KEYS
  )

  # ── Step 13: Apply decimals and name style ──────────────────────────────────
  if (!is.null(decimals)) {
    result <- .apply_decimals(result, decimals)
  }
  .apply_name_style(result, name_style)
}
