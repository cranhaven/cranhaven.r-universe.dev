# R/glm-anova-dispatch.R
#
# Polymorphic-input dispatcher for get_anova(). The numerical kernel lives in
# R/glm-anova.R (.get_anova_single()); this file routes the user-facing
# `object` argument to the correct internal path.
#
# Dispatch branches:
#   survey_glm_fit          -> single-model anova (sequential)
#   list<survey_glm_fit>    -> chained pairwise comparisons (k fits => k-1 rows)
#   survey_base             -> fit via survey_glm(), then single-model anova
#   otherwise               -> surveycore_error_anova_object_invalid
#
# Error classes (A-21..A-25) defined in plans/error-messages.md.

#' Design-Based Analysis of Variance and Wald Tests for Survey GLM Fits
#'
#' Rao-Scott design-based ANOVA and design-based Wald tests for
#' [survey_glm()] fits. Accepts three input shapes on `object`:
#'
#' * A single [survey_glm_fit] — sequential mode, one row per term.
#' * A list of [survey_glm_fit] objects — chained pairwise comparison,
#'   producing `length(object) - 1` rows.
#' * A survey design (any [survey_base] subclass) — fits the model internally
#'   via [survey_glm()] using `formula` (or `response` + `predictors`), then
#'   runs sequential anova on the fit.
#'
#' Supports the four method x test combinations shared with
#' `survey::anova.svyglm()`: Rao-Scott working-LRT with F or Chisq reference,
#' and design-based Wald with F or Chisq reference.
#'
#' @param object A [survey_glm_fit], a list of [survey_glm_fit] objects, or a
#'   survey design ([survey_base] subclass).
#' @param formula A model formula (e.g. `y ~ x1 + x2`). Only used when
#'   `object` is a survey design. Passed through to [survey_glm()]; supplying
#'   `formula` alongside `response` / `predictors` is rejected by
#'   [survey_glm()]'s validator.
#' @param response Character string naming the outcome variable. Only used
#'   when `object` is a survey design. Forwarded to [survey_glm()].
#' @param predictors Character vector of predictor variable names. Only used
#'   when `object` is a survey design. Forwarded to [survey_glm()].
#' @param ... Additional arguments forwarded to [survey_glm()] when `object`
#'   is a survey design (e.g. `family`, `na.action`, `quiet`). For fit or list
#'   inputs, `...` must be empty — any extras error via
#'   [rlang::check_dots_empty()] with fuzzy typo detection.
#' @param method Character(1). `"LRT"` (default) or `"Wald"`.
#' @param test Character(1). `"F"` (default) or `"Chisq"` reference
#'   distribution.
#' @param null Numeric or `NULL`. Hypothesized value for the tested
#'   coefficients (Wald only). Only used when `object` is a single
#'   [survey_glm_fit] or a survey design (reducing to single-model mode);
#'   ignored with warning `surveycore_warning_anova_null_ignored` when
#'   `object` is a list of fits.
#' @param tolerance Numeric(1). Reciprocal-condition-number threshold for the
#'   naive-covariance near-singular gate in the Rao-Scott LRT. Default
#'   `sqrt(.Machine$double.eps)`.
#' @param decimals Integer(1) or `NULL`. Round double output columns.
#' @param label_vars Logical(1). When `TRUE`, compose term-row labels from
#'   `@metadata@variable_labels` for the `term` column. Default `TRUE`.
#' @param name_style Character(1). `"surveycore"` (default) or `"broom"`.
#'
#' @return A `survey_anova` tibble with columns `term`, `statistic`, `df`,
#'   `ddf`, `deff`, `p_value`, `stars` and a `.meta` attribute. When
#'   `name_style = "broom"`, `p_value` is renamed to `p.value` and `ddf`
#'   is renamed to `df.residual`.
#'
#' @examples
#' gss_cc <- gss_2024[stats::complete.cases(gss_2024[, c("age", "sex", "educ")]), ]
#' gss_design <- as_survey(
#'   gss_cc,
#'   ids = vpsu,
#'   weights = wtssps,
#'   strata = vstrat,
#'   nest = TRUE
#' )
#'
#' # Single fit
#' fit <- survey_glm(gss_design, age ~ sex + educ)
#' get_anova(fit)
#'
#' # Design + formula (fits internally)
#' get_anova(gss_design, age ~ sex + educ)
#'
#' # List of fits (chained pairwise comparison)
#' fit_s <- survey_glm(gss_design, age ~ sex)
#' fit_b <- survey_glm(gss_design, age ~ sex + educ)
#' get_anova(list(fit_s, fit_b))
#' @family analysis
#' @export
get_anova <- function(
  object,
  formula = NULL,
  response = NULL,
  predictors = NULL,
  ...,
  method = c("LRT", "Wald"),
  test = c("F", "Chisq"),
  null = NULL,
  tolerance = sqrt(.Machine$double.eps),
  decimals = NULL,
  label_vars = TRUE,
  name_style = "surveycore"
) {
  if (S7::S7_inherits(object, survey_collection)) {
    fn_name <- "get_anova"
    cli::cli_abort(
      c(
        "x" = "{.fn {fn_name}} does not yet support {.cls survey_collection} inputs.",
        "i" = paste0(
          "Run {.fn {fn_name}} on each survey individually, or see ",
          "{.topic survey_collection} for the current dispatch coverage."
        )
      ),
      class = "surveycore_error_collection_not_supported_by_fn"
    )
  }
  cl <- match.call()

  has_formula_args <-
    !is.null(formula) || !is.null(response) || !is.null(predictors)

  is_fit <- S7::S7_inherits(object, survey_glm_fit)
  is_design <- S7::S7_inherits(object, survey_base)
  is_list_input <- is.list(object) && !S7::S7_inherits(object)

  if (!is_fit && !is_list_input && !is_design) {
    cli::cli_abort(
      c(
        "x" = "{.arg object} must be a {.cls survey_glm_fit}, a list of {.cls survey_glm_fit} objects, or a survey design.",
        "i" = "Got {.cls {class(object)[[1L]]}}."
      ),
      class = "surveycore_error_anova_object_invalid"
    )
  }

  # Formula-args only apply to design inputs. Fit / list branches reject.
  if (!is_design && has_formula_args) {
    cli::cli_abort(
      c(
        "x" = "{.arg formula} / {.arg response} / {.arg predictors} must be {.code NULL} when {.arg object} is a {.cls survey_glm_fit} or a list of fits.",
        "i" = "These arguments only apply when {.arg object} is a survey design.",
        "v" = "Drop the formula argument, or pass a design as {.arg object} instead."
      ),
      class = "surveycore_error_anova_formula_unexpected"
    )
  }

  # `...` is forwarded to survey_glm() for design inputs; errors for fit/list.
  if (!is_design) {
    rlang::check_dots_empty()
  }

  method <- match.arg(method)
  test <- match.arg(test)

  if (is_fit) {
    return(
      .get_anova_single(
        model = object,
        model2 = NULL,
        method = method,
        test = test,
        null = null,
        tolerance = tolerance,
        decimals = decimals,
        label_vars = label_vars,
        name_style = name_style,
        call = cl
      )
    )
  }

  if (is_list_input) {
    return(
      .anova_dispatch_list(
        fits = object,
        method = method,
        test = test,
        null = null,
        tolerance = tolerance,
        decimals = decimals,
        label_vars = label_vars,
        name_style = name_style,
        call = cl
      )
    )
  }

  # Design path: fit via survey_glm(), then run single-model anova.
  .anova_dispatch_design(
    design = object,
    formula = formula,
    response = response,
    predictors = predictors,
    ...,
    method = method,
    test = test,
    null = null,
    tolerance = tolerance,
    decimals = decimals,
    label_vars = label_vars,
    name_style = name_style,
    call = cl
  )
}


# -- .anova_dispatch_list() ----------------------------------------------------

# Iterate a list<survey_glm_fit>, delegating to .get_anova_single() on each
# consecutive pair. For k == 1 unwraps silently to single-model anova. For
# k >= 2 produces k-1 rows; the resulting tibble is a vertical bind of each
# pair's output, sharing one `.meta` built from the first pair (with an
# extended `$models` entry enumerating every fit in the chain).
.anova_dispatch_list <- function(
  fits,
  method,
  test,
  null,
  tolerance,
  decimals,
  label_vars,
  name_style,
  call
) {
  k <- length(fits)
  if (k == 0L) {
    cli::cli_abort(
      c(
        "x" = "{.arg object} is a zero-length list.",
        "v" = "Supply at least one {.cls survey_glm_fit}."
      ),
      class = "surveycore_error_anova_empty_model_list"
    )
  }

  bad_idx <- which(
    !vapply(
      fits,
      function(f) S7::S7_inherits(f, survey_glm_fit),
      logical(1L)
    )
  )
  if (length(bad_idx) > 0L) {
    bad_classes <- vapply(
      fits[bad_idx],
      function(f) class(f)[[1L]],
      character(1L)
    )
    cli::cli_abort(
      c(
        "x" = "All elements of {.arg object} must inherit {.cls survey_glm_fit}.",
        "i" = "Bad element(s) at position(s) {.val {bad_idx}}: {.cls {bad_classes}}."
      ),
      class = "surveycore_error_anova_object_invalid"
    )
  }

  # Type-check `null` before the list-mode warning: a wrong-type null is a
  # contract violation regardless of mode (Q7 validation order).
  if (!is.null(null) && !is.numeric(null)) {
    cli::cli_abort(
      c(
        "x" = "{.arg null} must be numeric or {.code NULL}.",
        "i" = "Got {.cls {class(null)[1]}}."
      ),
      class = "surveycore_error_null_length_mismatch"
    )
  }

  if (k == 1L) {
    return(
      .get_anova_single(
        model = fits[[1L]],
        model2 = NULL,
        method = method,
        test = test,
        null = null,
        tolerance = tolerance,
        decimals = decimals,
        label_vars = label_vars,
        name_style = name_style,
        call = call
      )
    )
  }

  # k >= 2: null is meaningless; warn and drop.
  if (!is.null(null)) {
    cli::cli_warn(
      c(
        "!" = "{.arg null} has no effect when {.arg object} is a list of fits.",
        "i" = "{.arg null} only applies to single-model anova.",
        "i" = "Dropping {.arg null} and continuing with model comparison."
      ),
      class = "surveycore_warning_anova_null_ignored"
    )
    null <- NULL
  }

  if (k == 2L) {
    return(
      .get_anova_single(
        model = fits[[1L]],
        model2 = fits[[2L]],
        method = method,
        test = test,
        null = null,
        tolerance = tolerance,
        decimals = decimals,
        label_vars = label_vars,
        name_style = name_style,
        call = call
      )
    )
  }

  # k >= 3: pairwise chain. Each pair produces a 1-row survey_anova; stack
  # them and rebuild a combined meta.
  pair_results <- lapply(seq_len(k - 1L), function(i) {
    .get_anova_single(
      model = fits[[i]],
      model2 = fits[[i + 1L]],
      method = method,
      test = test,
      null = null,
      tolerance = tolerance,
      decimals = decimals,
      label_vars = label_vars,
      name_style = name_style,
      call = call
    )
  })

  # Stack tibble rows. rbind on tibbles drops per-column attributes; copy them
  # back from the first pair so downstream consumers (gt, print) see the
  # same `label` / `labels_by_row` attributes as single-fit output.
  result_class <- class(pair_results[[1L]])
  first <- pair_results[[1L]]
  stacked <- do.call(
    rbind,
    lapply(pair_results, function(r) {
      class(r) <- setdiff(class(r), c("survey_anova", "survey_result"))
      r
    })
  )
  class(stacked) <- result_class
  for (nm in names(stacked)) {
    src_attrs <- attributes(first[[nm]])
    lbl <- src_attrs$label
    if (!is.null(lbl)) {
      attr(stacked[[nm]], "label") <- lbl
    }
  }

  # Combined meta: base on the first pair, extend `terms` with every pair's
  # term meta, and add a `$models` list enumerating all k fits' formula/family
  # summaries for introspection.
  base_meta <- attr(pair_results[[1L]], ".meta")
  combined_terms <- unlist(
    lapply(pair_results, function(r) attr(r, ".meta")$terms),
    recursive = FALSE
  )
  base_meta$terms <- combined_terms
  base_meta$call <- call
  base_meta$models <- lapply(fits, function(f) {
    fam_name <- if (inherits(f@family, "family")) {
      f@family$family
    } else {
      f@family$family %||% NA_character_
    }
    link_name <- if (inherits(f@family, "family")) {
      f@family$link
    } else {
      f@family$link %||% NA_character_
    }
    list(
      formula = f@formula,
      family = fam_name,
      link = link_name,
      n_obs = as.integer(length(f@fitted_values)),
      coefficients = names(f@coefficients)
    )
  })
  attr(stacked, ".meta") <- base_meta
  class(stacked) <- result_class

  stacked
}


# -- .anova_dispatch_design() --------------------------------------------------

# Fit the model via survey_glm(), then run single-model anova on the fit.
# Missing-formula validation fires at this layer so the error reports
# `get_anova()` (not `survey_glm()`) as the scope.
.anova_dispatch_design <- function(
  design,
  formula,
  response,
  predictors,
  ...,
  method,
  test,
  null,
  tolerance,
  decimals,
  label_vars,
  name_style,
  call
) {
  if (is.null(formula) && is.null(response) && is.null(predictors)) {
    cli::cli_abort(
      c(
        "x" = "{.arg formula} (or {.arg response} / {.arg predictors}) is required when {.arg object} is a survey design.",
        "v" = "Supply a model formula, e.g. {.code get_anova(design, y ~ x1 + x2)}."
      ),
      class = "surveycore_error_anova_formula_missing"
    )
  }

  fit <- survey_glm(
    design = design,
    formula = formula,
    response = response,
    predictors = predictors,
    ...
  )

  .get_anova_single(
    model = fit,
    model2 = NULL,
    method = method,
    test = test,
    null = null,
    tolerance = tolerance,
    decimals = decimals,
    label_vars = label_vars,
    name_style = name_style,
    call = call
  )
}
