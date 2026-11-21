# DESCRIPTIVE COHORT TABLE STATISTICAL ENGINE
# Primary compute engine for table1-shaped descriptive and baseline cohort specifications.
# Computes raw frequencies, continuous summaries, hypothesis tests, SMDs, and covariate-adjusted effects.

#########
# DESCRIPTIVE ENGINE DISPATCH
# Orchestrates variable summaries, hypothesis tests, SMDs, and effect estimations.

#' Execute descriptive analysis engine on a specification and dataset
#' @keywords internal
#' @noRd
.engine_descriptive <- function(spec, data) {
  args <- .descriptive_args_from_spec(spec)
  vars <- args$vars
  strat <- args$strat
  analysis_vars <- unique(c(vars, strat))
  analysis_vars <- analysis_vars[!is.na(analysis_vars) & nzchar(analysis_vars)]
  for (v in analysis_vars) {
    value <- data[[v]]
    if ((!is.atomic(value) && !is.factor(value)) || !is.null(dim(value))) {
      simtab_abort_binding(c(
        "Variable {.val {v}} must be an atomic vector or factor.",
        "i" = "Received a column of class {.cls {class(value)[[1]]}}.",
        "v" = "List columns and matrix columns cannot be summarised; flatten the column first."
      ))
    }
  }
  test <- args$test
  p_adjust <- args$p.adjust
  paired <- isTRUE(args$paired)
  smd <- isTRUE(args$smd)
  effect_name <- args$effect
  adjust.for <- args$adjust.for
  conf.level <- args$conf.level
  denominator <- args$denominator
  model_na <- args$model_na

  # Conditioning an effect of X on Y upon Y itself is never a covariate
  # adjustment: the model becomes degenerate and reports a meaningless estimate
  # (often exactly 1, or a diverged one) with a confident-looking interval. Drop
  # it rather than fitting it, and say so, since a broad tidyselect adjustment
  # can pick the grouping column up by accident.
  if (!is.null(strat) && !is.null(adjust.for) && strat %in% adjust.for) {
    warning(
      sprintf(
        paste0(
          "Adjustment variable '%s' is the grouping variable, so it was dropped from the ",
          "covariate set; an effect cannot be adjusted for its own outcome."
        ),
        strat
      ),
      call. = FALSE
    )
    adjust.for <- setdiff(adjust.for, strat)
    if (length(adjust.for) == 0) {
      adjust.for <- NULL
    }
  }

  complete_vars <- analysis_vars
  complete_idx <- if (length(complete_vars) == 0) {
    rep(TRUE, nrow(data))
  } else {
    stats::complete.cases(data[, complete_vars, drop = FALSE])
  }
  display_data <- if (identical(denominator, "complete")) {
    data[complete_idx, , drop = FALSE]
  } else {
    data
  }

  slevs <- NULL
  event_lev <- NULL
  if (!is.null(strat)) {
    sv <- factor(display_data[[strat]])
    slevs <- levels(droplevels(sv))
    if (!is.null(effect_name)) {
      event_lev <- slevs[2]
    }
  }

  deferred <- character(0)
  test_notes <- list()
  smd_group_note <- FALSE

  groups <- .compute_groups(display_data, strat, args$overall)
  var_labels <- vapply(vars, function(v) .resolve_label(v, data, args$labels), character(1))
  names(var_labels) <- vars

  measure <- if (is.null(spec$effect$measure)) NULL else .get_measure(spec$effect$measure)
  family <- if (is.null(measure)) NULL else .measure_family(measure)

  model_n <- integer(0)
  effect_estimators <- character(0)
  effect_measure_meta <- list()
  logbinomial_fallback <- logical(0)
  effect_converged <- logical(0)
  effect_boundary <- logical(0)
  logbinomial_converged <- logical(0)
  logbinomial_boundary <- logical(0)
  logbinomial_status <- character(0)
  summary_auto <- list()
  var_data <- list()
  any_test <- FALSE
  ref_found <- logical(0)
  for (v in vars) {
    xv <- display_data[[v]]
    vtype <- .detect_var_type(xv, override = .resolve_per_var(args$var.type, v, NULL))
    stat_requested <- tolower(.resolve_per_var(args$stat.cont, v, "auto"))
    if (!stat_requested %in% c("auto", "median", "mean")) {
      simtab_abort_input(c(
        "{.arg stat.cont} for {.val {v}} must be {.val auto}, {.val median}, or {.val mean}.",
        "i" = "Received: {.val {stat_requested}}.",
        "v" = "Use {.val auto} to let the skewness heuristic choose."
      ))
    }
    refv <- .resolve_per_var(args$ref, v, NULL)

    rec <- list(type = vtype, label = var_labels[[v]])

    if (identical(vtype, "continuous")) {
      if (!is.numeric(xv)) {
        simtab_abort_binding(c(
          "Variable {.val {v}} is not numeric.",
          "i" = "It was resolved as continuous but has class {.cls {class(xv)[[1]]}}.",
          "v" = "Convert it, or set {.code var.type = \"categorical\"}."
        ))
      }
      y <- as.numeric(xv)
      .check_finite_continuous(y, v)
      auto_decision <- NULL
      stat_v <- stat_requested
      if (identical(stat_requested, "auto")) {
        auto_decision <- .auto_summary_decision(y)
        stat_v <- auto_decision$decision
        summary_auto[[v]] <- auto_decision
      }
      summ <- list()
      cnts <- integer(0)
      nmiss <- integer(0)
      for (g in groups$names) {
        idx <- groups$idx[[g]]
        yg <- y[idx]
        ok <- !is.na(yg)
        summ[[g]] <- .summ_cont(yg[ok], stat_v)
        cnts[g] <- sum(ok)
        nmiss[g] <- sum(!ok)
      }
      rec$stat <- stat_v
      rec$stat_requested <- stat_requested
      rec$auto_summary <- auto_decision
      rec$summary <- summ
      rec$counts <- cnts
      rec$n_missing <- nmiss
    } else {
      vf <- factor(xv)
      if (!is.null(refv)) {
        ref_found[[v]] <- as.character(refv) %in% levels(vf)
      }
      if (!is.null(refv) && as.character(refv) %in% levels(vf)) {
        vf <- stats::relevel(vf, ref = as.character(refv))
      }
      vf <- droplevels(vf)
      vlevs <- levels(vf)
      freq <- matrix(
        0L,
        nrow = length(vlevs),
        ncol = length(groups$names),
        dimnames = list(vlevs, groups$names)
      )
      pct <- matrix(
        NA_real_,
        nrow = length(vlevs),
        ncol = length(groups$names),
        dimnames = list(vlevs, groups$names)
      )
      nmiss <- integer(0)
      for (g in groups$names) {
        idx <- groups$idx[[g]]
        vg <- vf[idx]
        denom <- sum(!is.na(vg))
        for (l in vlevs) {
          freq[l, g] <- sum(vg == l, na.rm = TRUE)
        }
        if (denom > 0) {
          pct[, g] <- freq[, g] / denom * 100
        }
        nmiss[g] <- sum(is.na(vg))
      }
      rec$levels <- vlevs
      rec$ref_level <- vlevs[1]
      rec$freq <- freq
      rec$pct <- pct
      rec$n_missing <- nmiss
    }

    rec$test <- NULL
    rec$smd <- NA_real_
    if (smd && !is.null(strat)) {
      if (length(slevs) == 2) {
        rec$smd <- .table1_smd(xv, display_data[[strat]], vtype)
      } else if (!smd_group_note) {
        deferred <- c(deferred, "Note: SMD requires exactly two groups; no SMD column was computed.")
        smd_group_note <- TRUE
      }
    }

    if ((isTRUE(test) || is.character(test)) && !is.null(strat) && length(slevs) >= 2) {
      tr <- .table1_test(xv, display_data[[strat]], vtype, stat_v, test, paired = paired)
      if (!is.null(tr)) {
        rec$test <- tr
        any_test <- TRUE
        if (!is.null(tr$simtab_note)) {
          test_notes[[length(test_notes) + 1L]] <- .with_campbell_variable(tr$simtab_note, v)
        }
      }
    }

    rec$crude <- NULL
    if (!is.null(measure)) {
      if (.is_function_measure(measure) && identical(vtype, "continuous")) {
        simtab_abort_engine(c(
          sprintf("Function-valued measure {.val %s} cannot be applied to continuous variable {.val %s}.", spec$effect$measure, v),
          "i" = "The registered estimator/CI function contract consumes categorical 2x2 cell counts.",
          "v" = "Use an unadjusted categorical exposure, or register an engine for a different computation contract."
        ))
      }
      if (identical(vtype, "continuous")) {
        m <- .descriptive_fit_effect(
          display_data, strat, v, NULL, family, conf.level, ref = NULL
        )
        if (!is.null(m)) {
          rec$crude <- data.frame(
            level = NA_character_, m[1, c("estimate", "lower", "upper", "p")], ref = FALSE, row.names = NULL
          )
        }
      } else {
        rec$crude <- .descriptive_crude_cat(
          display_data[[v]], display_data[[strat]], event_lev, measure, conf.level, refv
        )
        custom_meta <- attr(rec$crude, "measure_meta", exact = TRUE)
        if (length(custom_meta) > 0) {
          effect_measure_meta[[v]] <- custom_meta
        }
        zero_cell_levels <- attr(rec$crude, "zero_cell_levels", exact = TRUE)
        if (length(zero_cell_levels) > 0) {
          test_notes[[length(test_notes) + 1L]] <- list(
            id = "zero_cell_correction",
            variable = v,
            levels = zero_cell_levels,
            measure = measure$label
          )
        }
      }
    }

    rec$adjusted <- NULL
    if (!is.null(adjust.for)) {
      covs <- setdiff(adjust.for, v)
      m <- .descriptive_fit_effect(
        data, strat, v, covs, family, conf.level,
        ref = if (identical(vtype, "continuous")) NULL else refv,
        model_na = model_na,
        measure = effect_name,
        use_pr_machine = toupper(effect_name %||% "") %in% c("PR", "RR")
      )
      if (!is.null(m)) {
        rec$adjusted_model_n <- attr(m, "model_n", exact = TRUE)
        rec$estimator_used <- attr(m, "estimator_used", exact = TRUE)
        rec$logbinomial_fallback <- isTRUE(attr(m, "logbinomial_fallback", exact = TRUE))
        rec$effect_converged <- attr(m, "converged", exact = TRUE)
        rec$effect_boundary <- attr(m, "boundary", exact = TRUE)
        rec$logbinomial_converged <- attr(m, "logbinomial_converged", exact = TRUE)
        rec$logbinomial_boundary <- attr(m, "logbinomial_boundary", exact = TRUE)
        rec$logbinomial_status <- attr(m, "logbinomial_status", exact = TRUE)
        if (!is.null(rec$estimator_used)) {
          effect_estimators[v] <- rec$estimator_used
        }
        logbinomial_fallback[v] <- rec$logbinomial_fallback
        effect_converged[v] <- rec$effect_converged %||% NA
        effect_boundary[v] <- rec$effect_boundary %||% NA
        logbinomial_converged[v] <- rec$logbinomial_converged %||% NA
        logbinomial_boundary[v] <- rec$logbinomial_boundary %||% NA
        logbinomial_status[v] <- rec$logbinomial_status %||% NA_character_
        if (!is.null(rec$adjusted_model_n) && !is.na(rec$adjusted_model_n)) {
          model_n[v] <- as.integer(rec$adjusted_model_n)
        }
        if (identical(vtype, "continuous")) {
          rec$adjusted <- data.frame(
            level = NA_character_, m[1, c("estimate", "lower", "upper", "p")], ref = FALSE, row.names = NULL
          )
        } else {
          lev <- c(rec$levels, setdiff(m$level[!is.na(m$level)], rec$levels))
          adj <- data.frame(
            level = lev,
            estimate = c(1, rep(NA_real_, length(lev) - 1)),
            lower = NA_real_,
            upper = NA_real_,
            p = NA_real_,
            ref = seq_along(lev) == 1
          )
          mi <- match(m$level, adj$level)
          ok <- !is.na(mi)
          adj$estimate[mi[ok]] <- m$estimate[ok]
          adj$lower[mi[ok]] <- m$lower[ok]
          adj$upper[mi[ok]] <- m$upper[ok]
          adj$p[mi[ok]] <- m$p[ok]
          rec$adjusted <- adj
        }
      }
    }

    var_data[[v]] <- rec
  }
  if (!is.null(measure)) {
    .descriptive_warn_unmatched_ref(args$ref, ref_found, vars)
  }
  var_data <- .table1_apply_p_adjust(var_data, p_adjust)

  meta <- list(
    vars = vars,
    labels = var_labels,
    strat_var = strat,
    strat_levels = slevs,
    event_level = event_lev,
    overall = args$overall,
    group_names = groups$names,
    group_n = groups$n,
    stat.cont = args$stat.cont,
    style = args$style,
    test = test,
    p.adjust = p_adjust,
    paired = paired,
    smd = smd,
    has_test = any_test,
    effect = effect_name,
    adjust = if (is.null(adjust.for)) NULL else list(vars = adjust.for, measure = effect_name),
    missing = list(display = args$missing, denominator = denominator, model_na = model_na),
    ref = args$ref,
    d = args$d,
    conf.level = conf.level,
    conf_pct = round(conf.level * 100),
    format = TRUE,
    n_total = nrow(display_data),
    n_available = nrow(data),
    complete_case_n = as.integer(sum(complete_idx)),
    model_n = model_n,
    effect_estimators = effect_estimators,
    effect_measure_meta = effect_measure_meta,
    logbinomial_fallback = logbinomial_fallback,
    effect_converged = effect_converged,
    effect_boundary = effect_boundary,
    logbinomial_converged = logbinomial_converged,
    logbinomial_boundary = logbinomial_boundary,
    logbinomial_status = logbinomial_status,
    summary_auto = summary_auto,
    test_notes = test_notes,
    patches = list(),
    args = args,
    engine = "descriptive"
  )

  for (msg in deferred) {
    message(msg)
  }

  list(data = var_data, meta = meta)
}

#########
# SPECIFICATION RESOLUTION AND P-VALUE ADJUSTMENT
# Resolves role bindings, continuous summaries, and applies multiplicity corrections.

#' Extract descriptive engine execution arguments from specification object
#' @keywords internal
#' @noRd
.descriptive_args_from_spec <- function(spec) {
  vars <- .resolve_tidyselect_role(spec, "describe")
  effect_name <- spec$effect$measure
  if (!is.null(effect_name)) {
    effect_name <- .get_measure(effect_name)$label
  }
  adjust.for <- .resolve_tidyselect_role(spec, "adjust")
  if (length(adjust.for) == 0) {
    adjust.for <- NULL
  }

  list(
    vars = vars,
    strat = .resolve_single_role(spec, "by"),
    overall = isTRUE(spec$layout$overall),
    stat.cont = .descriptive_stat_arg(spec, vars),
    var.type = spec$engine_opts$descriptive$var.type,
    style = spec$style,
    test = .comparison_test_arg(spec$comparison$test),
    p.adjust = spec$comparison$p.adjust %||% "none",
    paired = isTRUE(spec$comparison$paired),
    smd = isTRUE(spec$comparison$smd),
    effect = effect_name,
    adjust.for = adjust.for,
    missing = isTRUE(spec$missing$display),
    ref = spec$effect$ref,
    labels = spec$fmt$labels,
    d = as.integer(spec$fmt$d %||% 1),
    conf.level = spec$effect$conf.level %||% 0.95,
    denominator = spec$missing$denominator %||% "available",
    model_na = spec$missing$model_na %||% "drop"
  )
}

#' Apply p-value multiplicity adjustments across tested cohort variables
#' @keywords internal
#' @noRd
.table1_apply_p_adjust <- function(var_data, method) {
  method <- method %||% "none"
  if (identical(method, "none")) {
    return(var_data)
  }
  has_test <- vapply(var_data, function(rec) !is.null(rec$test), logical(1))
  if (!any(has_test)) {
    return(var_data)
  }
  raw <- vapply(var_data[has_test], function(rec) rec$test$p.value, numeric(1))
  adjusted <- stats::p.adjust(raw, method = method)
  tested_vars <- names(raw)
  for (i in seq_along(tested_vars)) {
    v <- tested_vars[[i]]
    var_data[[v]]$test$p.value.raw <- raw[[i]]
    var_data[[v]]$test$p.value.adjusted <- adjusted[[i]]
    var_data[[v]]$test$p.adjust.method <- method
  }
  var_data
}

#' Resolve continuous summary statistics for cohort variables
#' @keywords internal
#' @noRd
.descriptive_stat_arg <- function(spec, vars) {
  overrides <- spec$summary$overrides
  if (is.null(overrides) || length(overrides) == 0) {
    return(spec$summary$default %||% "auto")
  }

  stats <- as.list(rep(spec$summary$default %||% "auto", length(vars)))
  names(stats) <- vars
  for (nm in intersect(names(overrides), vars)) {
    stats[[nm]] <- overrides[[nm]]
  }
  stats
}

#' Normalize comparison test argument into engine flag or method name
#' @keywords internal
#' @noRd
.comparison_test_arg <- function(test) {
  if (is.null(test)) {
    return(FALSE)
  }
  test <- tolower(test)
  if (identical(test, "auto")) {
    return(TRUE)
  }
  test
}

#########
# EFFECT MEASURE ESTIMATION AND MODEL FITTING
# Fits crude 2x2 effects and multivariable GLM / log-binomial models with robust fallbacks.

#' Lookup GLM family object corresponding to an effect measure estimator
#' @keywords internal
#' @noRd
.measure_family <- function(measure) {
  if (is.function(measure$estimator)) {
    return(NULL)
  }
  estimator <- tolower(measure$estimator)
  switch(
    estimator,
    "binomial(logit)" = stats::binomial("logit"),
    "poisson(log)" = stats::poisson("log"),
    "logbinomial>robust-poisson" = stats::poisson("log"),
    simtab_abort_engine(c(
      "Unsupported measure estimator {.val {measure$estimator}}.",
      "i" = "The descriptive engine fits binomial(logit), poisson(log), or
             log-binomial with robust-Poisson fallback.",
      "v" = .measure_engine_hint(measure)
    ))
  )
}

#' Check if an effect measure relies on custom R functions
#' @keywords internal
#' @noRd
.is_function_measure <- function(measure) {
  is.function(measure$estimator) || is.function(measure$ci)
}

#' Fit adjusted effect model using log-binomial or robust Poisson GLM
#' @keywords internal
#' @noRd
.descriptive_fit_effect <- function(data,
                                    outcome,
                                    predictor,
                                    covars,
                                    family,
                                    conf.level,
                                    ref = NULL,
                                    model_na = "drop",
                                    measure = NULL,
                                    use_pr_machine = FALSE) {
  out <- if (isTRUE(use_pr_machine)) {
    .fit_pr_effect(
      data,
      outcome = outcome,
      focus = predictor,
      covars = covars,
      conf.level = conf.level,
      ref = ref,
      model_na = model_na
    )
  } else {
    .fit_glm_effect(
      data,
      outcome = outcome,
      focus = predictor,
      covars = covars,
      family = family,
      vcov = "HC0",
      conf.level = conf.level,
      ref = ref,
      model_na = model_na
    )
  }
  if (nrow(out) == 0) {
    return(NULL)
  }
  fit_attrs <- c(
    "model_n", "estimator_used", "logbinomial_fallback", "converged", "boundary",
    "logbinomial_converged", "logbinomial_boundary", "logbinomial_status"
  )
  trimmed <- out[c("level", "estimate", "lower", "upper", "p")]
  for (a in fit_attrs) {
    attr(trimmed, a) <- attr(out, a, exact = TRUE)
  }
  trimmed
}

#' Compute crude categorical 2x2 association measures across variable levels
#' @keywords internal
#' @noRd
.descriptive_crude_cat <- function(xv, sv, event_lev, measure, conf.level, refv) {
  vf <- factor(xv)
  if (!is.null(refv) && as.character(refv) %in% levels(vf)) {
    vf <- stats::relevel(vf, ref = as.character(refv))
  }
  vf <- droplevels(vf)
  vlevs <- levels(vf)
  out <- data.frame(
    level = vlevs,
    estimate = NA_real_,
    lower = NA_real_,
    upper = NA_real_,
    p = NA_real_,
    ref = FALSE
  )
  out$ref[1] <- TRUE
  ref_lev <- vlevs[1]
  a_ref <- sum(vf == ref_lev & sv == event_lev, na.rm = TRUE)
  n_ref <- sum(vf == ref_lev & !is.na(sv), na.rm = TRUE)
  if (n_ref > 0) {
    out$estimate[1] <- 1
  }

  calc <- .crude_ci_calculator(measure)
  corrected_levels <- character(0)
  measure_meta <- list()
  for (i in seq_along(vlevs)[-1]) {
    a_i <- sum(vf == vlevs[i] & sv == event_lev, na.rm = TRUE)
    n_i <- sum(vf == vlevs[i] & !is.na(sv), na.rm = TRUE)
    res <- calc(a_i, n_i, a_ref, n_ref, conf.level)
    if (!is.null(res$null)) {
      out$estimate[1] <- res$null
    }
    out$estimate[i] <- res$estimate
    out$lower[i] <- res$lower
    out$upper[i] <- res$upper
    out$p[i] <- res$p
    if (isTRUE(res$corrected)) {
      corrected_levels <- c(corrected_levels, vlevs[i])
    }
    if (length(res$meta %||% list()) > 0) {
      measure_meta[[vlevs[i]]] <- res$meta
    }
  }
  attr(out, "zero_cell_levels") <- corrected_levels
  attr(out, "measure_meta") <- measure_meta
  out
}

#' Resolve 2x2 confidence interval calculation function for an effect measure
#' @keywords internal
#' @noRd
.crude_ci_calculator <- function(measure) {
  function_contract <- c(is.function(measure$estimator), is.function(measure$ci))
  if (any(function_contract)) {
    if (!all(function_contract)) {
      simtab_abort_engine(c(
        sprintf("Function-valued measure {.val %s} has an incomplete computation contract.", measure$name),
        "i" = "The descriptive engine requires both {.arg estimator} and {.arg ci} to be functions.",
        "v" = "Register the paired functions documented by {.fn register_measure}."
      ))
    }
    return(function(a_index, n_index, a_ref, n_ref, conf.level) {
      .compute_function_measure_2x2(
        measure, a_index, n_index, a_ref, n_ref, conf.level
      )
    })
  }

  ci <- tolower(measure$ci)
  switch(
    ci,
    woolf = .calc_or_woolf,
    katz = .calc_pr_katz,
    simtab_abort_engine(c(
      "Unsupported crude CI method {.val {measure$ci}}.",
      "i" = "Crude 2x2 intervals use {.val woolf} (odds) or {.val katz} (risk).",
      "v" = .measure_engine_hint(measure)
    ))
  )
}

#########
# CUSTOM 2X2 MEASURE COMPUTATION AND VALIDATION
# Dispatches user-registered custom 2x2 effect measures and validates contract compliance.

#' Execute custom registered 2x2 estimator and confidence interval functions
#' @keywords internal
#' @noRd
.compute_function_measure_2x2 <- function(measure,
                                          a_index,
                                          n_index,
                                          a_ref,
                                          n_ref,
                                          conf.level) {
  estimator <- tryCatch(
    measure$estimator(a_index, n_index, a_ref, n_ref),
    error = function(e) {
      simtab_abort_engine(c(
        sprintf("Registered estimator for measure {.val %s} failed.", measure$name),
        "i" = conditionMessage(e),
        "v" = "Return the raw named-list contract documented by {.fn register_measure}."
      ))
    }
  )
  .validate_measure_function_result(estimator, "estimator", measure$name, c("estimate"))
  .validate_optional_measure_fields(estimator, "estimator", measure$name)

  interval <- tryCatch(
    measure$ci(
      estimator$estimate,
      a_index,
      n_index,
      a_ref,
      n_ref,
      conf.level
    ),
    error = function(e) {
      simtab_abort_engine(c(
        sprintf("Registered CI function for measure {.val %s} failed.", measure$name),
        "i" = conditionMessage(e),
        "v" = "Return the raw named-list contract documented by {.fn register_measure}."
      ))
    }
  )
  .validate_measure_function_result(interval, "ci", measure$name, c("lower", "upper"))
  .validate_optional_measure_fields(interval, "ci", measure$name)

  list(
    estimate = estimator$estimate,
    lower = interval$lower,
    upper = interval$upper,
    p = interval$p %||% estimator$p %||% NA_real_,
    null = estimator$null %||% 1,
    corrected = isTRUE(estimator$corrected) || isTRUE(interval$corrected),
    meta = utils::modifyList(estimator$meta %||% list(), interval$meta %||% list())
  )
}

#' Validate required numeric fields in custom measure output
#' @keywords internal
#' @noRd
.validate_measure_function_result <- function(x, source, measure, required) {
  valid <- is.list(x) && all(required %in% names(x)) &&
    all(vapply(x[required], function(value) is.numeric(value) && length(value) == 1, logical(1)))
  if (!valid) {
    simtab_abort_engine(c(
      sprintf("Registered %s for measure {.val %s} returned a malformed result.", source, measure),
      "i" = sprintf("Required scalar numeric field(s): %s.", paste(required, collapse = ", ")),
      "v" = "Return raw values using the contract documented by {.fn register_measure}."
    ))
  }
  invisible(x)
}

#' Validate optional return fields in custom measure output
#' @keywords internal
#' @noRd
.validate_optional_measure_fields <- function(x, source, measure) {
  numeric_fields <- intersect(c("null", "p"), names(x))
  numeric_ok <- all(vapply(x[numeric_fields], function(value) {
    is.numeric(value) && length(value) == 1
  }, logical(1)))
  corrected_ok <- is.null(x$corrected) ||
    (is.logical(x$corrected) && length(x$corrected) == 1 && !is.na(x$corrected))
  meta_ok <- is.null(x$meta) || is.list(x$meta)
  if (!numeric_ok || !corrected_ok || !meta_ok) {
    simtab_abort_engine(c(
      sprintf("Registered %s for measure {.val %s} returned malformed optional fields.", source, measure),
      "i" = "{.val null} and {.val p} must be scalar numeric, {.val corrected} scalar logical, and {.val meta} a list.",
      "v" = "Return raw values using the contract documented by {.fn register_measure}."
    ))
  }
  invisible(x)
}

#' Warn when a requested reference level was never applied
#'
#' A scalar `ref` is shared by every categorical variable, so it only needs to
#' match one of them; a named per-variable `ref` must match its own variable.
#' Otherwise the first level is used silently, which can invert a comparison.
#' @keywords internal
#' @noRd
.descriptive_warn_unmatched_ref <- function(ref, ref_found, vars) {
  if (is.null(ref)) {
    return(invisible(NULL))
  }
  per_var <- !is.null(names(ref))
  if (per_var) {
    unknown <- setdiff(names(ref), vars)
    missing_level <- names(ref_found)[!ref_found]
    problems <- c(
      if (length(unknown) > 0) sprintf("'%s' is not a described variable", unknown),
      vapply(missing_level, function(v) {
        sprintf("'%s' is not a level of '%s'", as.character(ref[[v]]), v)
      }, character(1))
    )
  } else {
    problems <- if (length(ref_found) > 0 && !any(ref_found)) {
      sprintf("'%s' is not a level of any categorical variable", as.character(ref))
    } else {
      character(0)
    }
  }
  if (length(problems) > 0) {
    warning(
      sprintf(
        "Reference level not applied (%s); the first level was used as the reference.",
        paste(unname(problems), collapse = "; ")
      ),
      call. = FALSE
    )
  }
  invisible(NULL)
}
