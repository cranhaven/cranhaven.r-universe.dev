#' One-step pass-rate weight calibration
#'
#' A convenience interface for everyday use. The user only supplies the data,
#' the outcome variable, the initial weights, an overall target and per-group
#' targets; the function builds the target table, identifies grouping
#' variables, runs the data checks and calls [calibrate_pass_rates()].
#'
#' @param data A data frame with one row per sampled unit.
#' @param outcome Name of the binary outcome column (1 = pass, 0 = fail).
#' @param weight Name of the initial weight column.
#' @param overall Optional scalar overall target rate; may be `NULL`.
#' @param groups Named list. Each element name is a grouping variable and each
#'   element is a named numeric vector of target rates.
#' @param priority Priority of the overall target. Defaults to 5.
#' @param group_priority Priority of the group targets; a single number or a
#'   vector named by grouping variable.
#' @param lower,upper Lower and upper bounds on the weight-adjustment multiplier.
#' @param mode `"soft"` for soft constraints, `"exact"` for exact constraints.
#' @param distance Calibration distance, passed to [calibrate_pass_rates()].
#'   `"chi2"` (default), `"raking"` (entropy) or `"logit"` (bounded); the latter
#'   two are exact mode only.
#' @param lambda Soft-constraint penalty strength.
#' @param new_weight Name of the new calibrated weight column.
#' @param check Whether to run the data checks before solving.
#' @param verbose Whether to print OSQP solver information.
#'
#' @return An object of class `pass_rate_calibration`.
#' @examples
#' d <- example_rate_data(300)
#' fit <- calibrate_rates(d, "qualified", "initial_weight",
#'                        groups = list(sex = c(M = 0.72, F = 0.68)))
#' summary(fit)
#' @export
calibrate_rates <- function(
    data,
    outcome,
    weight,
    overall = NULL,
    groups = list(),
    priority = 5,
    group_priority = 1,
    lower = 0.25,
    upper = 4,
    mode = c("soft", "exact"),
    distance = c("chi2", "raking", "logit"),
    lambda = 1e4,
    new_weight = "weight_calibrated",
    check = TRUE,
    verbose = FALSE
) {
  mode <- match.arg(mode)
  distance <- match.arg(distance)
  if (!is.list(groups) || is.null(names(groups)) || any(names(groups) == "")) {
    stop("groups must be a named list, e.g. list(sex = c(M = 0.7, F = 0.68)).",
         call. = FALSE)
  }
  group_vars <- names(groups)
  if (length(group_vars) < 1L) {
    stop("groups must contain at least one grouping variable.", call. = FALSE)
  }

  targets <- make_rate_targets(
    overall = overall,
    groups = groups,
    overall_priority = priority,
    group_priority = group_priority
  )

  if (isTRUE(check)) {
    report <- check_calibration_data(
      data = data,
      outcome = outcome,
      weight = weight,
      group_vars = group_vars,
      targets = targets
    )
    if (!isTRUE(report$ok)) {
      stop(paste(c("Data checks failed:", paste0("- ", report$errors)), collapse = "\n"),
           call. = FALSE)
    }
    if (length(report$warnings) > 0L) {
      warning(paste(c("Data check warnings:", paste0("- ", report$warnings)), collapse = "\n"),
              call. = FALSE)
    }
  }

  calibrate_pass_rates(
    data = data,
    outcome = outcome,
    weight = weight,
    group_vars = group_vars,
    targets = targets,
    lower = lower,
    upper = upper,
    mode = mode,
    distance = distance,
    lambda = lambda,
    new_weight = new_weight,
    verbose = verbose
  )
}

#' Pre-calibration data checks
#'
#' Checks variables, weights, the binary outcome, group coverage and target
#' supportability, and reports the current weighted pass rates.
#'
#' @param data A data frame.
#' @param outcome Name of the binary outcome column.
#' @param weight Name of the initial weight column.
#' @param group_vars Character vector of grouping-variable names.
#' @param targets Optional target table.
#' @param consistency_tol Tolerance (on the rate scale) for the overall-vs-group
#'   consistency warning. Only inconsistencies larger than this are reported, so
#'   that sub-tolerance rounding (round-number targets that do not divide the
#'   weighted marginals exactly) does not trigger noise. For a precise,
#'   exact-mode feasibility analysis call [calibration_feasibility()] directly.
#' @param x A `ratecalib_check` object (for the print method).
#' @param ... Further arguments (ignored by the print method).
#'
#' @return A list of class `ratecalib_check` with `ok`, `errors`, `warnings`,
#'   `overview`, `group_summary` and `target_support`.
#' @examples
#' d <- example_rate_data(300)
#' check_calibration_data(d, "qualified", "initial_weight", group_vars = "sex")
#' @export
check_calibration_data <- function(data, outcome, weight, group_vars,
                                   targets = NULL, consistency_tol = 0.01) {
  errors <- character()
  warnings <- character()

  if (!is.data.frame(data)) {
    return(list(ok = FALSE, errors = "data must be a data frame.", warnings = character()))
  }
  required <- unique(c(outcome, weight, group_vars))
  missing <- setdiff(required, names(data))
  if (length(missing)) errors <- c(errors, paste0("Missing columns: ", paste(missing, collapse = ", ")))
  if (length(errors)) return(list(ok = FALSE, errors = errors, warnings = warnings))

  w <- suppressWarnings(as.numeric(data[[weight]]))
  y_raw <- data[[outcome]]
  if (is.logical(y_raw)) y_raw <- as.integer(y_raw)
  y <- suppressWarnings(as.numeric(as.character(y_raw)))

  if (anyNA(w) || any(!is.finite(w))) errors <- c(errors, "Initial weights contain missing or non-finite values.")
  if (any(w <= 0, na.rm = TRUE)) errors <- c(errors, "All initial weights must be greater than 0.")
  if (anyNA(y) || any(!y %in% c(0, 1))) errors <- c(errors, "The outcome column must contain only 0 and 1.")
  if (anyNA(data[group_vars])) errors <- c(errors, "Grouping variables contain missing values; recode missing values as an explicit category first.")
  if (length(errors)) return(list(ok = FALSE, errors = errors, warnings = warnings))

  weighted_rate <- function(mask) sum(w[mask] * y[mask]) / sum(w[mask])
  overview <- data.frame(
    metric = c("sample_size", "initial_weight_total", "initial_weighted_rate",
               "initial_weight_min", "initial_weight_median", "initial_weight_max"),
    value = c(nrow(data), sum(w), weighted_rate(rep(TRUE, nrow(data))), min(w), stats::median(w), max(w)),
    check.names = FALSE
  )

  group_summary <- data.frame()
  for (v in group_vars) {
    levs <- unique(as.character(data[[v]]))
    for (lev in levs) {
      mask <- as.character(data[[v]]) == lev
      yy <- y[mask]
      row <- data.frame(
        variable = v,
        level = lev,
        n = sum(mask),
        initial_weight_total = sum(w[mask]),
        initial_weighted_rate = weighted_rate(mask),
        has_0 = any(yy == 0),
        has_1 = any(yy == 1),
        stringsAsFactors = FALSE
      )
      group_summary <- rbind(group_summary, row)
      if (!row$has_0 || !row$has_1) {
        warnings <- c(warnings, paste0(v, " = ", lev, " contains only ",
                                       if (row$has_1) "passing" else "failing",
                                       " units; its target rate cannot be changed by reweighting within the group."))
      }
    }
  }

  target_support <- NULL
  if (!is.null(targets)) {
    targets <- as.data.frame(targets, stringsAsFactors = FALSE)
    needed <- c("variable", "level", "target_rate")
    if (!all(needed %in% names(targets))) {
      errors <- c(errors, "targets must contain the columns variable, level and target_rate.")
    } else {
      target_support <- targets
      target_support$supported <- TRUE
      target_support$reason <- ""
      # The outcome-based support check only applies to simple
      # proportion-of-outcome targets on a single dimension. Interaction keys,
      # mean/total targets and proportions of a non-outcome value are left
      # marked supported with an explanatory reason rather than misjudged.
      stat_col <- if ("statistic" %in% names(targets)) as.character(targets$statistic) else rep("proportion", nrow(targets))
      stat_col[is.na(stat_col) | stat_col == ""] <- "proportion"
      vv_col <- if ("value_var" %in% names(targets)) as.character(targets$value_var) else rep(NA_character_, nrow(targets))
      val_col <- if ("value" %in% names(targets)) as.character(targets$value) else rep(NA_character_, nrow(targets))
      for (i in seq_len(nrow(targets))) {
        v <- as.character(targets$variable[i])
        lev <- as.character(targets$level[i])
        r <- as.numeric(targets$target_rate[i])
        simple <- stat_col[i] == "proportion" &&
          (is.na(vv_col[i]) || vv_col[i] == "" || vv_col[i] == outcome) &&
          (is.na(val_col[i]) || val_col[i] == "1") &&
          !grepl(":", v, fixed = TRUE)
        if (!simple) {
          target_support$reason[i] <- "not checked (interaction or non-outcome statistic target)"
          next
        }
        if (v %in% c(".overall", "overall", "TOTAL")) {
          mask <- rep(TRUE, nrow(data))
        } else if (!v %in% group_vars) {
          target_support$supported[i] <- FALSE
          target_support$reason[i] <- "target variable is not in group_vars"
          next
        } else {
          mask <- as.character(data[[v]]) == lev
        }
        if (!any(mask)) {
          target_support$supported[i] <- FALSE
          target_support$reason[i] <- "no sample in this category"
        } else if (all(y[mask] == 0) && r > 0) {
          target_support$supported[i] <- FALSE
          target_support$reason[i] <- "group is all 0; a target above 0 is unreachable"
        } else if (all(y[mask] == 1) && r < 1) {
          target_support$supported[i] <- FALSE
          target_support$reason[i] <- "group is all 1; a target below 1 is unreachable"
        }
      }
      bad <- which(!target_support$supported)
      if (length(bad)) {
        errors <- c(errors, paste0("There are ", length(bad),
                                   " target(s) not supported by the data; see target_support."))
      }
    }
  }

  # Overall-vs-group consistency: deterministic, closed-form. Informational
  # only (a conflict is fatal in exact mode but merely unachievable in soft
  # mode), so it is reported as a warning rather than an error.
  if (!is.null(targets) && all(c("variable", "level", "target_rate") %in% names(targets))) {
    fz <- tryCatch(
      calibration_feasibility(data, outcome, weight, group_vars, targets,
                              tol = consistency_tol),
      error = function(e) NULL
    )
    if (!is.null(fz) && !isTRUE(fz$consistency$consistent)) {
      warnings <- c(warnings, paste0(
        "Targets imply conflicting overall rates (", fz$consistency$detail,
        "); under mode='exact' this is infeasible, under mode='soft' the targets ",
        "cannot all be met. See calibration_feasibility()."))
    }
  }

  structure(list(
    ok = length(errors) == 0L,
    errors = unique(errors),
    warnings = unique(warnings),
    overview = overview,
    group_summary = group_summary,
    target_support = target_support
  ), class = "ratecalib_check")
}

#' @rdname check_calibration_data
#' @export
print.ratecalib_check <- function(x, ...) {
  cat("ratecalib pre-calibration check\n")
  cat("Status: ", if (isTRUE(x$ok)) "passed" else "failed", "\n", sep = "")
  if (length(x$errors)) {
    cat("\nErrors:\n", paste0("- ", x$errors, collapse = "\n"), "\n", sep = "")
  }
  if (length(x$warnings)) {
    cat("\nWarnings:\n", paste0("- ", x$warnings, collapse = "\n"), "\n", sep = "")
  }
  if (!is.null(x$overview)) {
    cat("\nData overview:\n")
    print(x$overview, row.names = FALSE)
  }
  invisible(x)
}

#' Generate example data
#'
#' Creates a simulated data set with sex, residence, a 5-level education
#' variable, a 5-level age variable, a pass indicator and initial weights.
#'
#' @param n Sample size.
#' @param seed Random seed.
#' @return A data frame.
#' @examples
#' d <- example_rate_data(200)
#' head(d)
#' @export
example_rate_data <- function(n = 5000L, seed = 2026L) {
  set.seed(seed)
  sex <- sample(c("M", "F"), n, TRUE, c(0.49, 0.51))
  residence <- sample(c("Urban", "Rural"), n, TRUE, c(0.65, 0.35))
  education5 <- sample(paste0("Edu", 1:5), n, TRUE, c(0.14, 0.23, 0.25, 0.27, 0.11))
  age5 <- sample(paste0("Age", 1:5), n, TRUE, c(0.20, 0.24, 0.23, 0.19, 0.14))
  eta <- 0.45 + 0.10 * (sex == "M") + 0.08 * (residence == "Urban") +
    0.09 * (as.integer(sub("Edu", "", education5)) - 3) -
    0.06 * (as.integer(sub("Age", "", age5)) - 3)
  p <- stats::plogis(eta)
  qualified <- stats::rbinom(n, 1, p)
  initial_weight <- exp(stats::rnorm(n, 0, 0.28))
  data.frame(sex, residence, education5, age5, qualified, initial_weight,
             stringsAsFactors = FALSE)
}
