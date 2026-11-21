# ROC AND AREA UNDER THE CURVE (AUC) COMPUTATION ENGINE
# Fits empirical ROC curves, DeLong confidence intervals, optimal cutpoints,
# and paired DeLong statistical comparisons across continuous diagnostic markers.

#########
# ROC ENGINE DISPATCH
# Coordinates data filtering, direction resolution, curve coordinates, and AUC summaries.

#' Execute ROC curve and AUC evaluation engine
#' @keywords internal
#' @noRd
.engine_roc <- function(spec, data) {
  .require_pkg("pROC", "roc()")

  args <- .roc_args_from_spec(spec)
  markers <- args$markers
  outcome_var <- args$outcome

  if (length(markers) == 0) {
    simtab_abort_binding(c(
      "{.fn roc} requires at least one marker.",
      "i" = "The {.arg marker} selection resolved to no columns.",
      "v" = "Select one or more continuous columns, e.g. {.code marker = troponin}."
    ))
  }
  if (is.null(outcome_var) || !outcome_var %in% names(data)) {
    simtab_abort_binding(c(
      "{.arg outcome} must resolve to a single column present in the data.",
      "i" = "Columns available: {.val {names(data)}}.",
      "v" = "Pass one bare column name, e.g. {.code outcome = acs}."
    ))
  }
  missing_markers <- setdiff(markers, names(data))
  if (length(missing_markers) > 0) {
    simtab_abort_binding(c(
      "Marker variable(s) not found in data: {.val {missing_markers}}.",
      "i" = "Columns available: {.val {names(data)}}.",
      "v" = "Check the spelling, or select existing columns."
    ))
  }
  non_numeric <- markers[!vapply(data[markers], function(x) is.numeric(x) && !is.factor(x), logical(1))]
  if (length(non_numeric) > 0) {
    simtab_abort_binding(c(
      "ROC markers must be continuous numeric variables: {.val {non_numeric}}.",
      "i" = "A ROC curve sweeps a threshold across a continuous scale.",
      "v" = "Use {.fn diag_test} for a binary index test."
    ))
  }

  keep_vars <- c(outcome_var, markers)
  ok <- stats::complete.cases(data[, keep_vars, drop = FALSE])
  n_missing <- sum(!ok)
  if (n_missing > 0) {
    message(sprintf(
      "Removed %d observation(s) with missing values (%.1f%%).",
      n_missing, 100 * n_missing / nrow(data)
    ))
  }
  used <- data[ok, keep_vars, drop = FALSE]
  if (nrow(used) == 0) {
    simtab_abort_engine(c(
      "No valid observations after removing missing values.",
      "i" = "Every row was missing the outcome or at least one marker.",
      "v" = "Check the selected columns for all-missing data."
    ))
  }

  outcome <- droplevels(factor(used[[outcome_var]]))
  levs <- levels(outcome)
  if (length(levs) != 2) {
    simtab_abort_binding(c(
      "{.arg outcome} must have exactly 2 levels, but has {length(levs)}.",
      "i" = "Levels found: {.val {levs}}.",
      "v" = "Collapse the outcome to a case/non-case pair."
    ))
  }

  candidates_ref <- c(
    "1", "Sim", "Yes", "Positivo", "Positive",
    "Doente", "Disease", "Case", "Event", "S", "Y",
    "TRUE", "True"
  )
  pos <- .resolve_pos_level(args$positive, levs, candidates_ref, "outcome", "positive")
  neg <- setdiff(levs, pos)[1]
  outcome <- factor(outcome, levels = c(neg, pos))

  roc_objects <- list()
  auc_rows <- list()
  cutpoint_rows <- list()
  curve_rows <- list()

  for (marker in markers) {
    roc_obj <- pROC::roc(
      response = outcome,
      predictor = used[[marker]],
      levels = c(neg, pos),
      direction = args$direction,
      quiet = TRUE
    )
    roc_objects[[marker]] <- roc_obj

    ci_auc <- .roc_ci_auc(roc_obj, args$conf.level)
    auc_rows[[marker]] <- data.frame(
      marker = marker,
      auc = as.numeric(pROC::auc(roc_obj)),
      conf.low = as.numeric(ci_auc[1]),
      conf.high = as.numeric(ci_auc[3]),
      ci_method = attr(ci_auc, "method") %||% args$ci,
      n = nrow(used),
      n_pos = sum(outcome == pos),
      n_neg = sum(outcome == neg),
      direction_used = roc_obj$direction
    )

    if (!identical(args$cutpoint, "none")) {
      cut <- pROC::coords(
        roc_obj,
        "best",
        best.method = args$cutpoint,
        ret = c("threshold", "sensitivity", "specificity", "ppv", "npv"),
        transpose = FALSE
      )
      cut <- as.data.frame(cut)
      cut$marker <- marker
      cut$youden_j <- cut$sensitivity + cut$specificity - 1
      cutpoint_rows[[marker]] <- cut[c(
        "marker", "threshold", "sensitivity", "specificity", "ppv", "npv", "youden_j"
      )]
    }

    curve <- pROC::coords(
      roc_obj,
      "all",
      ret = c("threshold", "sensitivity", "specificity"),
      transpose = FALSE
    )
    curve <- as.data.frame(curve)
    curve$marker <- marker
    curve_rows[[marker]] <- curve[c("marker", "threshold", "sensitivity", "specificity")]
  }

  comparisons <- .roc_pairwise_comparisons(roc_objects)
  auc <- do.call(rbind, auc_rows)
  rownames(auc) <- NULL
  cutpoints <- if (length(cutpoint_rows) == 0) {
    data.frame(
      marker = character(), threshold = numeric(), sensitivity = numeric(),
      specificity = numeric(), ppv = numeric(), npv = numeric(),
      youden_j = numeric()
    )
  } else {
    out <- do.call(rbind, cutpoint_rows)
    rownames(out) <- NULL
    out
  }
  curve <- do.call(rbind, curve_rows)
  rownames(curve) <- NULL

  list(
    data = list(
      auc = auc,
      cutpoints = cutpoints,
      curve = curve,
      comparisons = comparisons
    ),
    meta = list(
      marker = markers,
      outcome = outcome_var,
      positive = pos,
      negative = neg,
      ci = args$ci,
      cutpoint = args$cutpoint,
      direction = args$direction,
      conf.level = args$conf.level,
      conf_pct = round(args$conf.level * 100),
      percent = isTRUE(args$percent),
      sample_sizes = auc[c("marker", "n", "n_pos", "n_neg")],
      engine = "roc",
      rule_flags = list(
        auto_direction = identical(args$direction, "auto"),
        cutpoint_present = nrow(cutpoints) > 0,
        apparent_auc = TRUE,
        min_class_n = min(auc$n_pos, auc$n_neg),
        n_pairwise_comparisons = nrow(comparisons)
      ),
      style = spec$style
    )
  )
}

#########
# SPECIFICATION EXTRACTION AND INFERENCE PRIMITIVES
# Resolves specification parameters, DeLong confidence intervals, and pairwise contrasts.

#' Extract ROC engine parameters and variable bindings from specification
#' @keywords internal
#' @noRd
.roc_args_from_spec <- function(spec) {
  roc_opts <- spec$engine_opts$roc %||% list()
  list(
    markers = .resolve_tidyselect_role(spec, "describe"),
    outcome = .resolve_single_role(spec, "ref_std"),
    positive = roc_opts$positive %||% NULL,
    direction = roc_opts$direction %||% "auto",
    ci = "delong",
    cutpoint = roc_opts$cutpoint %||% "youden",
    conf.level = roc_opts$conf.level %||% 0.95,
    percent = roc_opts$percent %||% spec$fmt$percent %||% FALSE
  )
}

#' Compute DeLong confidence interval for area under the curve
#' @keywords internal
#' @noRd
.roc_ci_auc <- function(roc_obj, conf.level) {
  pROC::ci.auc(roc_obj, method = "delong", conf.level = conf.level)
}

#' Compute paired DeLong tests between multiple ROC curves
#' @keywords internal
#' @noRd
.roc_pairwise_comparisons <- function(roc_objects) {
  markers <- names(roc_objects)
  if (length(markers) < 2) {
    return(data.frame(
      pair = character(), marker_1 = character(), marker_2 = character(),
      auc_1 = numeric(), auc_2 = numeric(), auc_diff = numeric(),
      se = numeric(), z = numeric(), p.value = numeric()
    ))
  }

  pairs <- utils::combn(markers, 2, simplify = FALSE)
  rows <- lapply(pairs, function(pair) {
    test <- pROC::roc.test(
      roc_objects[[pair[[1]]]],
      roc_objects[[pair[[2]]]],
      method = "delong",
      paired = TRUE
    )
    auc_1 <- as.numeric(test$estimate[[1]])
    auc_2 <- as.numeric(test$estimate[[2]])
    z <- as.numeric(test$statistic[[1]])
    diff <- auc_1 - auc_2
    data.frame(
      pair = paste(pair, collapse = " vs "),
      marker_1 = pair[[1]],
      marker_2 = pair[[2]],
      auc_1 = auc_1,
      auc_2 = auc_2,
      auc_diff = diff,
      se = if (is.finite(z) && z != 0) abs(diff / z) else NA_real_,
      z = z,
      p.value = as.numeric(test$p.value)
    )
  })

  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}
