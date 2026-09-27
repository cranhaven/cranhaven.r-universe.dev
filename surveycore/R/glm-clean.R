# R/glm-clean.R
#
# Phase 2: clean() — tidy survey_glm_fit into a survey_glm_tidy result tibble
#
# Contents:
#   .glm_term_type()          — classify each model term
#   .glm_find_level()         — find factor level for a coefficient name
#   .glm_var_label_for()      — get variable label or fallback to variable name
#   .glm_value_label_for()    — get value label for a factor level name
#   .glm_n_obs_for()          — compute n_obs for a term row
#   .glm_interaction_label()  — build label for interaction terms
#   .build_glm_meta()         — build .meta list for survey_glm_tidy
#   clean()                   — main exported function
#   tidy.survey_glm_fit()     — broom shim (not exported; registered in zzz.R)
#
# Spec: plans/spec-phase-2.md §VI

# ── .glm_term_type() ──────────────────────────────────────────────────────────
#
# Classify a term label as one of:
#   "intercept", "continuous", "factor", "ordered_factor",
#   "interaction", "transformation"
#
# @param term_label Character(1): one entry from attr(terms(fit), "term.labels")
#   or "(Intercept)".
# @param mf         The model frame (data.frame from model.frame(fit)).
# @return Character(1) type string.
#' @noRd
.glm_term_type <- function(term_label, mf) {
  if (identical(term_label, "(Intercept)")) {
    return("intercept")
  }
  if (grepl(":", term_label, fixed = TRUE)) {
    return("interaction")
  }
  if (term_label %in% names(mf)) {
    col <- mf[[term_label]]
    if (is.ordered(col)) {
      return("ordered_factor")
    }
    if (is.factor(col)) {
      return("factor")
    }
    return("continuous")
  }
  return("transformation")
}


# ── .glm_find_level() ─────────────────────────────────────────────────────────
#
# Find the factor level name for a coefficient name. Uses exact match via
# paste0(var_name, l) == coef_name — no string prefix removal (per spec §6.3).
#
# @param coef_name Character(1): coefficient name from model matrix,
#                  e.g. "sexMale".
# @param var_name  Character(1): the bare variable name, e.g. "sex".
# @param lvls      Character vector of factor levels.
# @return Character(1): the level name, or NA_character_ if not found.
#' @noRd
.glm_find_level <- function(coef_name, var_name, lvls) {
  for (l in lvls) {
    if (identical(paste0(var_name, l), coef_name)) return(l)
  }
  NA_character_
}


# ── .glm_var_label_for() ──────────────────────────────────────────────────────
#
# Get the variable label for a variable, falling back to the variable name.
#
# @param design   A survey_base object.
# @param var_name Character(1): variable name.
# @return Character(1): label or variable name.
#' @noRd
.glm_var_label_for <- function(design, var_name) {
  lbl <- design@metadata@variable_labels[[var_name]]
  if (is.null(lbl) || is.na(lbl)) var_name else lbl
}


# ── .glm_value_label_for() ────────────────────────────────────────────────────
#
# Get the display label for a factor level. Currently always returns
# level_name: in haven convention, factor level names are themselves the
# display labels (they are stored as the names of the val_labels vector,
# not as separate label strings). Both branches return level_name.
#
# @param design     A survey_base object.
# @param var_name   Character(1): variable name.
# @param level_name Character(1): factor level name.
# @return Character(1): level_name (the level name is the display label).
.glm_value_label_for <- function(design, var_name, level_name) {
  val_labels <- design@metadata@value_labels[[var_name]]
  if (!is.null(val_labels) && level_name %in% names(val_labels)) {
    level_name
  } else {
    level_name
  }
}


# ── .glm_interaction_label() ──────────────────────────────────────────────────
#
# Build the label for an interaction term coefficient. Joins component labels
# with interaction_sep. Factor components use level name / value label.
#
# @param coef_name       Character(1): coefficient name, e.g. "age:sexMale".
# @param term_label      Character(1): term label, e.g. "age:sex".
# @param mf              Model frame.
# @param design          survey_base object.
# @param interaction_sep Character(1): separator.
# @return Character(1): interaction label.
#' @noRd
.glm_interaction_label <- function(
  coef_name,
  term_label,
  mf,
  design,
  interaction_sep
) {
  comps_term <- strsplit(term_label, ":", fixed = TRUE)[[1L]]
  comps_coef <- strsplit(coef_name, ":", fixed = TRUE)[[1L]]

  if (length(comps_term) != length(comps_coef)) {
    # Fallback: use raw coef name if structure is unexpected
    return(coef_name)
  }

  pieces <- mapply(
    function(tc, cc) {
      if (tc %in% names(mf)) {
        col <- mf[[tc]]
        if (is.factor(col) || is.ordered(col)) {
          lvl <- .glm_find_level(cc, tc, levels(col))
          if (!is.na(lvl)) .glm_value_label_for(design, tc, lvl) else cc
        } else {
          .glm_var_label_for(design, tc)
        }
      } else {
        tc # transformation component
      }
    },
    comps_term,
    comps_coef,
    SIMPLIFY = TRUE,
    USE.NAMES = FALSE
  )

  paste(pieces, collapse = interaction_sep)
}


# ── .glm_n_obs_for() ──────────────────────────────────────────────────────────
#
# Compute the unweighted observation count for a term row.
#
# @param term_type  Character(1): from .glm_term_type().
# @param var_name   Character(1): parent variable name.
# @param coef_name  Character(1): coefficient name (for factor level detection).
# @param term_label Character(1): full term label (for interaction).
# @param mf         Model frame.
# @return Integer(1).
#' @noRd
.glm_n_obs_for <- function(term_type, var_name, coef_name, term_label, mf) {
  if (term_type == "intercept") {
    return(as.integer(nrow(mf)))
  }
  if (term_type %in% c("continuous", "ordered_factor")) {
    if (var_name %in% names(mf)) {
      return(as.integer(sum(!is.na(mf[[var_name]]))))
    }
    return(as.integer(nrow(mf)))
  }
  if (term_type == "factor") {
    lvl <- .glm_find_level(coef_name, var_name, levels(mf[[var_name]]))
    if (!is.na(lvl)) {
      return(as.integer(sum(mf[[var_name]] == lvl, na.rm = TRUE)))
    }
    return(as.integer(nrow(mf)))
  }
  if (term_type == "interaction") {
    comps_term <- strsplit(term_label, ":", fixed = TRUE)[[1L]]
    comps_coef <- strsplit(coef_name, ":", fixed = TRUE)[[1L]]
    masks <- mapply(
      function(tc, cc) {
        if (tc %in% names(mf)) {
          col <- mf[[tc]]
          if (is.factor(col) || is.ordered(col)) {
            lvl <- .glm_find_level(cc, tc, levels(col))
            if (!is.na(lvl)) mf[[tc]] == lvl else !is.na(mf[[tc]])
          } else {
            !is.na(mf[[tc]])
          }
        } else {
          rep(TRUE, nrow(mf))
        }
      },
      comps_term,
      comps_coef,
      SIMPLIFY = FALSE
    )
    combined <- Reduce("&", masks)
    return(as.integer(sum(combined, na.rm = TRUE)))
  }
  # transformation
  as.integer(nrow(mf))
}


# ── .build_glm_meta() ─────────────────────────────────────────────────────────
#
# Construct the .meta list for a survey_glm_tidy result.
#
# @param model            A survey_glm_fit object.
# @param conf_level       Numeric(1): confidence level.
# @param call             Language: the clean() call.
# @param include_reference Logical: include_reference argument.
# @param exponentiate     Logical: exponentiate argument.
# @param mf              Model frame.
# @param mm              Model matrix.
# @param tt              Terms object.
# @return Named list with all 15 required keys.
#' @noRd
.build_glm_meta <- function(
  model,
  conf_level,
  call,
  include_reference,
  exponentiate,
  mf,
  mm,
  tt
) {
  design <- model@design
  term_labels <- attr(tt, "term.labels") # excludes intercept

  # Predictor variables: bare column names in mf (no interactions or transforms)
  pred_vars <- term_labels[term_labels %in% names(mf)]

  # Get contrasts from model matrix attribute
  mm_contrasts <- attr(mm, "contrasts")

  vars_meta <- stats::setNames(vector("list", length(pred_vars)), pred_vars)
  for (v in pred_vars) {
    col <- mf[[v]]

    var_lbl <- design@metadata@variable_labels[[v]]
    if (is.null(var_lbl) || is.na(var_lbl)) {
      var_lbl <- v
    }

    var_class <- class(col)[[1L]]
    var_type <- if (is.ordered(col)) {
      "ordered"
    } else if (is.factor(col)) {
      "categorical"
    } else {
      "continuous"
    }

    var_nlevels <- if (is.factor(col)) length(levels(col)) else NULL

    contr_val <- if (v %in% names(mm_contrasts)) {
      ct <- mm_contrasts[[v]]
      if (is.character(ct)) ct else "contr.treatment"
    } else if (is.factor(col) && !is.ordered(col)) {
      "contr.treatment"
    } else {
      NULL
    }

    ref_lev <- if (is.factor(col) && !is.ordered(col)) {
      ref_candidates <- setdiff(levels(col), colnames(stats::contrasts(col)))
      if (length(ref_candidates) > 0L) ref_candidates[[1L]] else NULL
    } else {
      NULL
    }

    val_labels_val <- design@metadata@value_labels[[v]]

    vars_meta[[v]] <- list(
      var_label = var_lbl,
      var_class = var_class,
      var_type = var_type,
      var_nlevels = var_nlevels,
      contrasts = contr_val,
      reference_level = ref_lev,
      value_labels = val_labels_val
    )
  }

  list(
    formula = model@formula,
    family = model@family$family,
    link = model@family$link,
    design_type = .glm_design_type_string(model@design),
    conf_level = conf_level,
    call = call,
    group_names = character(0L),
    group_labels = NULL,
    n_observations = as.integer(nrow(mm)),
    n_weighted = sum(model@weights),
    degf = model@degf,
    exponentiate = exponentiate,
    include_reference = include_reference,
    converged = model@converged,
    variables = vars_meta
  )
}


# ── clean() ───────────────────────────────────────────────────────────────────

#' Tidy a Survey GLM Fit
#'
#' Converts a `survey_glm_fit` object into a `survey_glm_tidy` result tibble
#' with one row per model coefficient (plus optional reference rows for factor
#' predictors), design-based standard errors, confidence intervals, and
#' structured metadata.
#'
#' @param model A `survey_glm_fit` object from [survey_glm()].
#' @param conf_level Numeric scalar in `(0, 1)`. Confidence level for
#'   confidence intervals. Default `0.95`.
#' @param include_reference Logical. If `TRUE`, reference levels for unordered
#'   factor predictors appear as rows with `estimate = NA` and
#'   `reference_row = TRUE`. Default `TRUE`.
#' @param n Logical. If `TRUE`, adds an `n_obs` column with the unweighted
#'   observation count per term. Default `FALSE`.
#' @param statistic Logical. If `TRUE` (default), includes the `statistic`
#'   (t-statistic) column. Set to `FALSE` to drop it.
#' @param exponentiate Logical. If `TRUE`, exponentiates `estimate`,
#'   `conf_low`, and `conf_high`. `std_error` is left on the log scale
#'   (matching `broom` convention). Fires
#'   `surveycore_warning_exponentiate_nonlog` when the model link is not
#'   log-based. Default `FALSE`.
#' @param interaction_sep Character scalar. Separator for interaction term
#'   labels. Default `" * "`.
#' @param ... Currently unused.
#'
#' @return A `survey_glm_tidy` object: a tibble with S3 class
#'   `c("survey_glm_tidy", "survey_result", "tbl_df", "tbl", "data.frame")`.
#'   Metadata is accessed via [meta()].
#'
#' @examples
#' d <- as_survey(
#'   gss_2024,
#'   ids = vpsu,
#'   weights = wtssps,
#'   strata = vstrat,
#'   nest = TRUE
#' )
#' fit <- survey_glm(d, age ~ sex)
#' clean(fit)
#' clean(fit, conf_level = 0.99, exponentiate = FALSE)
#' @seealso [survey_glm()] to fit the model, [meta()] to access metadata.
#' @family analysis
#' @export
clean <- function(
  model,
  conf_level = 0.95,
  include_reference = TRUE,
  n = FALSE,
  statistic = TRUE,
  exponentiate = FALSE,
  interaction_sep = " * ",
  ...
) {
  # ── Validation ────────────────────────────────────────────────────────────

  if (!S7::S7_inherits(model, survey_glm_fit)) {
    cli::cli_abort(
      c(
        "x" = paste0(
          "{.arg model} must be a {.cls survey_glm_fit} object, ",
          "not {.cls {class(model)[[1L]]}}."
        ),
        "v" = "Create a model with {.fn survey_glm}."
      ),
      class = "surveycore_error_not_glm_fit"
    )
  }

  if (
    !is.numeric(conf_level) ||
      length(conf_level) != 1L ||
      is.na(conf_level) ||
      conf_level <= 0 ||
      conf_level >= 1
  ) {
    cli::cli_abort(
      c(
        "x" = "{.arg conf_level} must be a single number in (0, 1).",
        "i" = "Got {.val {conf_level}}."
      ),
      class = "surveycore_error_invalid_conf_level"
    )
  }

  # ── exponentiate warning ─────────────────────────────────────────────────

  if (exponentiate) {
    log_links <- c("log", "logit", "cloglog", "cauchit")
    if (!model@family$link %in% log_links) {
      cli::cli_warn(
        c(
          "!" = paste0(
            "{.arg exponentiate = TRUE} with a non-log link ",
            "({.val {model@family$link}}) may produce ",
            "uninterpretable estimates."
          )
        ),
        class = "surveycore_warning_exponentiate_nonlog"
      )
    }
  }

  # ── Capture call ──────────────────────────────────────────────────────────

  cl <- match.call()

  # ── Model components ──────────────────────────────────────────────────────

  fit <- model@fit_
  mf <- stats::model.frame(fit)
  mm <- stats::model.matrix(fit)
  tt <- stats::terms(fit)
  term_labels <- c("(Intercept)", attr(tt, "term.labels"))
  mm_assign <- attr(mm, "assign")
  coef_names <- names(model@coefficients)
  estimates <- model@coefficients
  se_vec <- sqrt(diag(model@vcov))
  p <- length(estimates)
  design <- model@design

  # CIs for all estimated coefficients
  ci_mat <- .glm_confint(estimates, se_vec, model@degf, p, conf_level)

  # t-statistics and p-values (log scale, unaffected by exponentiate)
  df_t <- max(1, model@degf - (p - 1L))
  tstat <- estimates / se_vec
  pval <- 2 * stats::pt(-abs(tstat), df = df_t)

  # ── Build rows ────────────────────────────────────────────────────────────

  rows <- vector("list", 0L)

  unique_terms <- unique(mm_assign)

  for (term_idx in unique_terms) {
    term_label <- term_labels[[term_idx + 1L]]
    term_type <- .glm_term_type(term_label, mf)

    # Indices of coefficients for this term in the model matrix
    coef_idx <- which(mm_assign == term_idx)

    # Reference row(s) for unordered factors (inserted before estimated rows)
    if (identical(term_type, "factor") && include_reference) {
      var_name <- term_label
      ref_levels <- setdiff(
        levels(mf[[var_name]]),
        colnames(stats::contrasts(mf[[var_name]]))
      )
      for (ref_lev in ref_levels) {
        ref_term <- paste0(var_name, ref_lev)
        ref_var_lbl <- .glm_var_label_for(design, var_name)
        ref_lbl <- .glm_value_label_for(design, var_name, ref_lev)
        ref_n_obs <- if (n) {
          as.integer(sum(mf[[var_name]] == ref_lev, na.rm = TRUE))
        } else {
          NULL
        }

        rows <- c(
          rows,
          list(list(
            term = ref_term,
            variable = var_name,
            var_label = ref_var_lbl,
            label = ref_lbl,
            reference_row = TRUE,
            estimate = NA_real_,
            std_error = NA_real_,
            statistic_val = NA_real_,
            p_value = NA_real_,
            conf_low = NA_real_,
            conf_high = NA_real_,
            n_obs = ref_n_obs
          ))
        )
      }
    }

    # Estimated coefficient rows for this term
    for (j in seq_along(coef_idx)) {
      ci <- coef_idx[[j]]
      cn <- coef_names[[ci]]

      # Determine variable, var_label, label
      if (identical(term_type, "intercept")) {
        var_nm <- "(Intercept)"
        var_lbl <- NA_character_
        lbl <- "(Intercept)"
      } else if (identical(term_type, "continuous")) {
        var_nm <- term_label
        var_lbl <- .glm_var_label_for(design, var_nm)
        lbl <- var_lbl
      } else if (identical(term_type, "factor")) {
        var_nm <- term_label
        var_lbl <- .glm_var_label_for(design, var_nm)
        lvl_name <- .glm_find_level(cn, var_nm, levels(mf[[var_nm]]))
        lbl <- if (!is.na(lvl_name)) {
          .glm_value_label_for(design, var_nm, lvl_name)
        } else {
          cn # fallback
        }
      } else if (identical(term_type, "ordered_factor")) {
        var_nm <- term_label
        var_lbl <- .glm_var_label_for(design, var_nm)
        lbl <- cn # polynomial contrast names kept as-is
      } else if (identical(term_type, "interaction")) {
        var_nm <- term_label
        var_lbl <- NA_character_
        lbl <- .glm_interaction_label(
          cn,
          term_label,
          mf,
          design,
          interaction_sep
        )
      } else {
        # transformation
        var_nm <- term_label
        var_lbl <- NA_character_
        lbl <- cn
      }

      # n_obs (only computed when n = TRUE)
      n_obs_val <- if (n) {
        .glm_n_obs_for(term_type, var_nm, cn, term_label, mf)
      } else {
        NULL
      }

      rows <- c(
        rows,
        list(list(
          term = cn,
          variable = var_nm,
          var_label = var_lbl,
          label = lbl,
          reference_row = FALSE,
          estimate = estimates[[ci]],
          std_error = se_vec[[ci]],
          statistic_val = tstat[[ci]],
          p_value = pval[[ci]],
          conf_low = ci_mat[[ci, "lower"]],
          conf_high = ci_mat[[ci, "upper"]],
          n_obs = n_obs_val
        ))
      )
    }
  }

  # ── Assemble tibble ───────────────────────────────────────────────────────

  nr <- length(rows)

  result_df <- tibble::tibble(
    term = vapply(rows, `[[`, character(1L), "term"),
    variable = vapply(rows, `[[`, character(1L), "variable"),
    var_label = vapply(rows, `[[`, character(1L), "var_label"),
    label = vapply(rows, `[[`, character(1L), "label"),
    reference_row = vapply(rows, `[[`, logical(1L), "reference_row"),
    estimate = vapply(rows, `[[`, double(1L), "estimate"),
    std_error = vapply(rows, `[[`, double(1L), "std_error")
  )

  if (statistic) {
    result_df$statistic <- vapply(rows, `[[`, double(1L), "statistic_val")
  }

  result_df$p_value <- vapply(rows, `[[`, double(1L), "p_value")
  result_df$conf_low <- vapply(rows, `[[`, double(1L), "conf_low")
  result_df$conf_high <- vapply(rows, `[[`, double(1L), "conf_high")

  if (n) {
    result_df$n_obs <- vapply(
      rows,
      function(r) {
        v <- r$n_obs
        if (is.null(v)) NA_integer_ else v
      },
      integer(1L)
    )
  }

  # ── exponentiate ─────────────────────────────────────────────────────────

  if (exponentiate) {
    result_df$estimate <- exp(result_df$estimate)
    result_df$conf_low <- exp(result_df$conf_low)
    result_df$conf_high <- exp(result_df$conf_high)
    # std_error left on log scale per broom convention
  }

  # ── .meta and class ───────────────────────────────────────────────────────

  meta_list <- .build_glm_meta(
    model,
    conf_level,
    cl,
    include_reference,
    exponentiate,
    mf,
    mm,
    tt
  )

  structure(
    result_df,
    .meta = meta_list,
    class = c("survey_glm_tidy", "survey_result", "tbl_df", "tbl", "data.frame")
  )
}


# ── tidy.survey_glm_fit() ─────────────────────────────────────────────────────
#
# broom compatibility shim. Not exported; registered conditionally in .onLoad().
#
# Note: broom uses conf.level (period), surveycore uses conf_level (underscore).
# exponentiate uses the same name in both.
#
#' @noRd
tidy.survey_glm_fit <- function(
  x,
  conf.int = TRUE,
  conf.level = 0.95,
  exponentiate = FALSE,
  ...
) {
  clean(
    model = x,
    conf_level = conf.level,
    exponentiate = exponentiate,
    ...
  )
}
