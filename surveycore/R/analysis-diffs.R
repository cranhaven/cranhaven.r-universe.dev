# R/analysis-diffs.R
#
# Treatment effect estimation for survey designs.
# Exported function: get_diffs()
#
# Internal helpers: analysis-diffs-helpers.R
# Shared helpers: analysis-helpers.R

# ── get_diffs() ───────────────────────────────────────────────────────────────

#' Treatment Effect Estimation for Survey Designs
#'
#' Estimates treatment effects (differences from a reference group) via
#' survey-weighted regression. Supports bivariate and multivariate models,
#' Gaussian and non-Gaussian families, and optional subgroup analysis.
#'
#' @param design A survey design object: `survey_taylor`,
#'   `survey_replicate`, `survey_twophase`, or `survey_nonprob`.
#' @param x <[`tidy-select`][tidyselect::language]> A single unquoted
#'   numeric variable name for the dependent variable. Must resolve to
#'   exactly one numeric column (continuous or 0/1 binary).
#' @param treats <[`tidy-select`][tidyselect::language]> A single unquoted
#'   variable name for the treatment/group variable. Must resolve to
#'   exactly one column with at least 2 unique levels. Coerced to factor
#'   if not already.
#' @param group <[`tidy-select`][tidyselect::language]> Optional subgroup
#'   variable(s) for interaction analysis. When provided, treatment effects
#'   are reported separately within each subgroup. Combined with any
#'   grouping set by `group_by()`. Default `NULL`.
#' @param covariates Character vector of additional model terms as strings.
#'   Supports interactions (`"age * gender"`), polynomials
#'   (`"poly(edu, 2)"`), and transformations (`"log(income)"`). When
#'   provided, forces the marginaleffects estimation path. Default `NULL`.
#' @param ref_level Character(1). Reference level of `treats` for
#'   comparisons. If `NULL` (default), the first factor level is used.
#'   Must match an existing level.
#' @param pval_adj Character(1) or `NULL`. P-value adjustment method
#'   passed to [stats::p.adjust()]. Options: `"holm"`, `"hochberg"`,
#'   `"hommel"`, `"bonferroni"`, `"BH"`, `"BY"`, `"fdr"`, `"none"`.
#'   `NULL` = no adjustment. When `group` is active, adjustment is applied
#'   independently within each group.
#' @param show_means Logical. If `TRUE` (default), includes a `mean`
#'   column and a reference row with `estimate = 0`. Subject to link-scale
#'   suppression (see Details).
#' @param show_pct_change Logical. If `TRUE`, includes a `pct_change`
#'   column: `estimate / reference_mean`. Subject to link-scale
#'   suppression (see Details). Default `FALSE`.
#' @param scale Character(1). `"ame"` (default): average marginal effects
#'   on the response scale. `"link"`: coefficients on the link scale.
#'   For Gaussian/identity models, both are identical. Case-sensitive.
#' @param variance `NULL` or a character vector of one or more of `"se"`,
#'   `"ci"`. Controls which uncertainty columns appear. Default `"ci"`.
#' @param conf_level Numeric(1) in (0, 1). Confidence level. Default
#'   `0.95`.
#' @param alpha Numeric(1) strictly between 0 and 1. Significance threshold
#'   used when `show_favorability = TRUE` to classify whether a difference
#'   is statistically significant. Uses strict `<` (p < alpha). Default
#'   `0.05`.
#' @param show_favorability Logical. If `TRUE`, appends two logical columns
#'   to the result: `favorable` (the difference is statistically significant
#'   and in the direction indicated by `higher_is` metadata on `x`) and
#'   `backlash` (significant but in the opposite direction). Requires
#'   `higher_is` metadata set on `x` via [set_higher_is()]; if not set,
#'   both columns are all `FALSE`. Default `FALSE`.
#' @param min_cell_n Integer(1). Minimum unweighted cell size before
#'   `surveycore_warning_small_cell` fires. Default `30L`.
#' @param n_weighted Logical. If `TRUE`, includes an `n_weighted` column
#'   with sum of weights per treatment level. Default `FALSE`.
#' @param decimals Integer(1) or `NULL`. If non-NULL, rounds numeric
#'   output columns. `pct_change` is rounded to `decimals + 2`. Default
#'   `NULL`.
#' @param na.rm Logical. If `TRUE` (default), rows with `NA` in `x`,
#'   `treats`, or `group` are dropped before fitting. If `FALSE`, `NA`
#'   values cause an error.
#' @param label_values Logical. If `TRUE` (default), the `treats` and
#'   `group` columns display value labels from metadata instead of raw
#'   codes. Output type is `factor` when labels are applied.
#' @param name_style `"surveycore"` (default) or `"broom"`. When
#'   `"broom"`, renames `se` to `std.error`, `ci_low` to `conf.low`,
#'   etc. The `mean` column is excluded from renaming.
#' @param ... Passed to [survey_glm()]. Common uses:
#'   `family = quasibinomial()`.
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
#' @details
#' ## Estimation Paths
#'
#' `get_diffs()` uses two estimation paths:
#'
#' - **Clean path** (bivariate Gaussian with no covariates and no group, OR
#'   any family with `scale = "link"`): extracts coefficients directly from
#'   `clean()`. The intercept is the reference group mean; treatment
#'   coefficients are differences from reference. When `scale = "link"` and
#'   the family is non-Gaussian, `mean` and `pct_change` are suppressed.
#' - **Marginaleffects path** (covariates, non-Gaussian with
#'   `scale = "ame"`, or group): uses `avg_slopes()` for estimates and
#'   `avg_predictions()` for means.
#'
#' ## Link-Scale Suppression
#'
#' When `scale = "link"` and the family is non-Gaussian, the `mean` and
#' `pct_change` columns are suppressed (omitted entirely). Link-scale
#' means are not substantively meaningful.
#'
#' ## P-Value Adjustment
#'
#' When `group` is active, p-value adjustment is applied independently
#' within each group. For global adjustment across all comparisons,
#' apply [stats::p.adjust()] to the result manually. Confidence intervals
#' reflect the specified `conf_level` and are not affected by p-value
#' adjustment.
#'
#' ## Degrees of Freedom
#'
#' All p-values and confidence intervals use the t-distribution with
#' design-based residual degrees of freedom, regardless of estimation
#' path.
#'
#' ## Non-Gaussian Models
#'
#' By default, non-Gaussian models report average marginal effects on
#' the response scale. Set `scale = "link"` for coefficients on the link
#' scale (e.g., log-odds for logistic regression).
#'
#' @return A `survey_diffs` tibble (also inheriting `survey_result`).
#'   Columns (in order): group columns (when active), treatment variable,
#'   `estimate`, `pct_change` (optional), `mean` (optional), `n`,
#'   `n_weighted` (optional), `se` (optional), `ci_low` (optional),
#'   `ci_high` (optional), `p_value`, `stars`, `favorable` (optional),
#'   `backlash` (optional). Use [meta()] to access design type, family,
#'   reference level, and other metadata.
#'
#' @examples
#' # Create survey design with treatment groups
#' set.seed(42)
#' df <- data.frame(
#'   id = 1:200,
#'   wt = runif(200, 0.5, 2),
#'   dv = rnorm(200, 50, 10),
#'   arm = factor(sample(c("Control", "A", "B"), 200, TRUE))
#' )
#' d <- as_survey(df, weights = wt)
#'
#' # Basic treatment effect
#' get_diffs(d, dv, arm)
#'
#' # With percentage change and p-value adjustment
#' get_diffs(d, dv, arm, show_pct_change = TRUE, pval_adj = "BH")
#' @family analysis
#' @export
get_diffs <- function(
  design,
  x,
  treats,
  group = NULL,
  covariates = NULL,
  ref_level = NULL,

  pval_adj = NULL,
  show_means = TRUE,
  show_pct_change = FALSE,
  scale = c("ame", "link"),
  variance = "ci",
  conf_level = 0.95,
  alpha = 0.05,
  show_favorability = FALSE,
  min_cell_n = 30L,
  n_weighted = FALSE,
  decimals = NULL,
  na.rm = TRUE,
  label_values = TRUE,
  name_style = "surveycore",
  ...,
  .id = NULL,
  .if_missing_var = NULL
) {
  if (S7::S7_inherits(design, survey_collection)) {
    return(.dispatch_over_collection(
      get_diffs,
      design,
      x = {{ x }},
      treats = {{ treats }},
      group = {{ group }},
      covariates = {{ covariates }},
      ...,
      .id = .id,
      .if_missing_var = .if_missing_var
    ))
  }
  # ── Step 1: Validate shared args ──────────────────────────────────────────
  .validate_shared_args(
    variance,
    conf_level,
    name_style,
    decimals = decimals,
    na.rm = na.rm,
    valid_variance = c("se", "ci")
  )

  # ── Step 1b: Validate alpha ───────────────────────────────────────────────
  if (
    !is.numeric(alpha) ||
      length(alpha) != 1L ||
      is.na(alpha) ||
      !is.finite(alpha) ||
      alpha <= 0 ||
      alpha >= 1
  ) {
    cli::cli_abort(
      c(
        "x" = paste0(
          "{.arg alpha} must be a single numeric value strictly between ",
          "0 and 1. Got {.val {alpha}}."
        )
      ),
      class = "surveycore_error_alpha_invalid"
    )
  }

  # ── Step 2: Validate design class ─────────────────────────────────────────
  .check_unsupported_class(design, "get_diffs")

  # ── Step 3: Resolve x, treats, group ──────────────────────────────────────
  x_sym <- rlang::ensym(x)
  x_name <- rlang::as_name(x_sym)
  treats_sym <- rlang::ensym(treats)
  treats_name <- rlang::as_name(treats_sym)

  if (!x_name %in% names(design@data)) {
    cli::cli_abort(
      c(
        "x" = "{.arg x} must select exactly one column.",
        "i" = "Column {.field {x_name}} not found in design data."
      ),
      class = "surveycore_error_wrong_variable_count"
    )
  }

  if (!treats_name %in% names(design@data)) {
    cli::cli_abort(
      c(
        "x" = "{.arg treats} must select exactly one column.",
        "i" = "Column {.field {treats_name}} not found in design data."
      ),
      class = "surveycore_error_treats_single"
    )
  }

  group_quo <- rlang::enquo(group)
  group_names <- .resolve_groups(design, group_quo)

  # ── Step 4: Validate x is numeric, treats has >= 2 levels ─────────────────
  x_col <- design@data[[x_name]]
  if (!is.numeric(x_col)) {
    cli::cli_abort(
      c(
        "x" = paste0(
          "{.arg x} must be numeric. ",
          "{.field {x_name}} is {.cls {class(x_col)[[1L]]}}."
        )
      ),
      class = "surveycore_error_non_numeric_variable"
    )
  }

  treats_col <- design@data[[treats_name]]
  if (!is.factor(treats_col)) {
    cli::cli_warn(
      c("!" = "{.field {treats_name}} coerced to factor."),
      class = "surveycore_warning_treats_coerced"
    )
    treats_col <- as.factor(treats_col)
    design@data[[treats_name]] <- treats_col
  }

  obs_levels <- unique(
    as.character(treats_col[!is.na(treats_col)])
  )
  if (length(obs_levels) < 2L) {
    cli::cli_abort(
      c(
        "x" = paste0(
          "{.arg treats} must have at least 2 levels. ",
          "{.field {treats_name}} has only {length(obs_levels)}."
        )
      ),
      class = "surveycore_error_treats_one_level"
    )
  }

  # ── Step 4a: Validate covariates + pval_adj ───────────────────────────────
  if (!is.null(covariates)) {
    if (!is.character(covariates)) {
      cli::cli_abort(
        c(
          "x" = paste0(
            "{.arg covariates} must be a character vector ",
            "of model terms."
          )
        ),
        class = "surveycore_error_covariates_not_character"
      )
    }
  }

  if (!is.null(pval_adj)) {
    valid_methods <- stats::p.adjust.methods
    if (!pval_adj %in% valid_methods) {
      cli::cli_abort(
        c(
          "x" = paste0(
            "{.arg pval_adj} must be a valid method for ",
            "{.fn stats::p.adjust}."
          ),
          "i" = paste0(
            "Valid methods: ",
            "{.or {.val {valid_methods}}}."
          )
        ),
        class = "surveycore_error_invalid_pval_adj"
      )
    }
  }

  # ── Step 5: Handle ref_level ──────────────────────────────────────────────
  treats_levels <- levels(treats_col)
  if (is.null(ref_level)) {
    ref_level <- treats_levels[[1L]]
  } else {
    if (!ref_level %in% treats_levels) {
      cli::cli_abort(
        c(
          "x" = paste0(
            "{.arg ref_level} {.val {ref_level}} not found ",
            "in levels of {.field {treats_name}}."
          ),
          "i" = paste0(
            "Available levels: ",
            "{.or {.val {treats_levels}}}."
          )
        ),
        class = "surveycore_error_ref_level_not_found"
      )
    }
  }

  # ── Step 6: na.rm -> na.action ────────────────────────────────────────────
  na_action <- if (na.rm) stats::na.omit else stats::na.fail

  # ── Step 7: Relevel + force contr.treatment ───────────────────────────────
  design@data[[treats_name]] <- stats::relevel(
    design@data[[treats_name]],
    ref = ref_level
  )
  stats::contrasts(design@data[[treats_name]]) <-
    stats::contr.treatment(
      levels(design@data[[treats_name]])
    )

  # ── Step 8: Build formula ─────────────────────────────────────────────────
  rhs_terms <- treats_name
  if (length(group_names) > 0L) {
    rhs_terms <- paste(
      c(treats_name, group_names),
      collapse = " * "
    )
  }
  if (!is.null(covariates)) {
    rhs_terms <- paste(
      c(rhs_terms, covariates),
      collapse = " + "
    )
  }
  formula <- stats::reformulate(rhs_terms, response = x_name)

  # ── Step 9: Fit model ────────────────────────────────────────────────────
  cl <- match.call()
  fit <- survey_glm(design, formula, na.action = na_action, ...)

  # ── Step 10: Determine estimation path ────────────────────────────────────
  scale <- match.arg(scale)
  family_name <- fit@family$family
  has_covariates <- !is.null(covariates)
  has_group <- length(group_names) > 0L

  use_marginaleffects <- has_covariates ||
    has_group ||
    (family_name != "gaussian" && scale == "ame")
  suppress_mean <- (scale == "link" && family_name != "gaussian")

  # ── Step 11: Extract estimates ────────────────────────────────────────────
  if (!use_marginaleffects) {
    estimate_method <- "coefficient"
    mean_method <- "intercept"
    estimate_scale <- "coefficient"
    result <- .extract_clean_estimates(fit, treats_name, conf_level)
  } else {
    estimate_method <- "avg_slopes"
    mean_method <- "avg_predictions"
    estimate_scale <- if (scale == "link") "coefficient" else "ame"
    result <- .extract_me_estimates(
      fit,
      treats_name,
      group_names,
      ref_level,
      scale,
      suppress_mean
    )
  }

  # ── Steps 12–17: Build output ─────────────────────────────────────────────
  domain_mask <- .apply_domain(design)
  wt_var <- design@variables$weights
  out <- .build_diffs_output(
    result = result,
    design = design,
    treats_name = treats_name,
    group_names = group_names,
    ref_level = ref_level,
    domain_mask = domain_mask,
    wt_var = wt_var,
    show_means = show_means,
    use_marginaleffects = use_marginaleffects,
    pval_adj = pval_adj,
    show_pct_change = show_pct_change,
    suppress_mean = suppress_mean,
    min_cell_n = min_cell_n,
    variance = variance,
    n_weighted = n_weighted
  )
  col_vecs <- out$col_vecs
  groups_df <- out$groups_df

  # ── Step 17a: Apply label_values ──────────────────────────────────────────
  label_df <- data.frame(
    col_vecs[[treats_name]],
    stringsAsFactors = FALSE
  )
  names(label_df) <- treats_name

  if (ncol(groups_df) > 0L) {
    for (gn in group_names) {
      label_df[[gn]] <- groups_df[[gn]]
    }
  }

  label_df <- .apply_group_labels(
    label_df,
    treats_name,
    design,
    label_values
  )
  col_vecs[[treats_name]] <- label_df[[treats_name]]

  if (ncol(groups_df) > 0L) {
    groups_df <- .apply_group_labels(
      groups_df,
      group_names,
      design,
      label_values
    )
  }

  # ── Step 18: Apply decimals ───────────────────────────────────────────────
  result <- tibble::as_tibble(c(as.list(groups_df), col_vecs))

  if (!is.null(decimals)) {
    has_pct <- "pct_change" %in% names(result)
    if (has_pct) {
      pct_saved <- result$pct_change
    }
    result <- .apply_decimals(result, decimals)
    if (has_pct) {
      result$pct_change <- round(pct_saved, decimals + 2L)
    }
  }

  # ── Step 19: Apply name_style ─────────────────────────────────────────────
  result <- .apply_name_style(result, name_style, exclude = "mean")

  # ── Step 20: Attach .meta ────────────────────────────────────────────────
  x_meta <- .extract_var_meta(design, x_name)

  # ── Step 20a: Favorability columns ───────────────────────────────────────
  if (isTRUE(show_favorability)) {
    higher_is_val <- x_meta$higher_is
    pval_col <- if ("p_value" %in% names(result)) "p_value" else "p.value"
    p_vals <- result[[pval_col]]
    sig <- !is.na(p_vals) & p_vals < alpha

    favorable <- logical(nrow(result))
    backlash <- logical(nrow(result))

    if (!is.null(higher_is_val)) {
      est <- result$estimate
      if (higher_is_val == "better") {
        favorable[sig] <- est[sig] > 0
        backlash[sig] <- est[sig] < 0
      } else if (higher_is_val == "worse") {
        favorable[sig] <- est[sig] < 0
        backlash[sig] <- est[sig] > 0
      }
    }

    attr(favorable, "label") <- "Favorable"
    attr(backlash, "label") <- "Backlash"

    saved_meta <- attr(result, ".meta")
    saved_class <- class(result)
    result$favorable <- favorable
    result$backlash <- backlash
    attr(result, ".meta") <- saved_meta
    class(result) <- saved_class
  }

  treats_meta <- .extract_var_meta(design, treats_name)
  treats_meta$name <- treats_name
  treats_meta$ref_level <- ref_level
  group_meta <- .build_group_meta(design, group_names)

  meta_args <- list(
    conf_level = conf_level,
    call = cl,
    group = group_meta,
    x = stats::setNames(list(x_meta), x_name),
    treats = treats_meta,
    covariates = covariates,
    family = family_name,
    link = fit@family$link,
    pval_adj = pval_adj,
    estimate_method = estimate_method,
    mean_method = mean_method,
    estimate_scale = estimate_scale
  )

  attr(result, ".meta") <- .build_meta(design, meta_args)

  # Validate meta keys
  stopifnot(all(DIFFS_META_KEYS %in% names(meta_args)))

  # ── Step 21: Column-level labels ──────────────────────────────────────────
  col_labels <- list(
    estimate = paste0("Difference relative to ", ref_level),
    pct_change = "% Change",
    mean = "Mean",
    n = "N",
    n_weighted = "N (weighted)",
    se = "Std. Error",
    ci_low = "Low CI",
    ci_high = "High CI",
    p_value = "P-Value",
    stars = ""
  )
  broom_labels <- list(
    std.error = "Std. Error",
    conf.low = "Low CI",
    conf.high = "High CI",
    p.value = "P-Value"
  )
  treats_label <- design@metadata@variable_labels[[treats_name]]
  if (is.null(treats_label)) {
    treats_label <- treats_name
  }

  for (cn in names(result)) {
    lbl <- NULL
    if (cn == treats_name) {
      lbl <- treats_label
    } else if (cn %in% group_names) {
      gl <- design@metadata@variable_labels[[cn]]
      lbl <- if (!is.null(gl)) gl else cn
    } else if (cn %in% names(col_labels)) {
      lbl <- col_labels[[cn]]
    } else if (cn %in% names(broom_labels)) {
      lbl <- broom_labels[[cn]]
    }
    if (!is.null(lbl)) {
      attr(result[[cn]], "label") <- lbl
    }
  }

  # ── Step 22: Set class and return ─────────────────────────────────────────
  class(result) <- c(
    "survey_diffs",
    "survey_result",
    "tbl_df",
    "tbl",
    "data.frame"
  )
  result
}
