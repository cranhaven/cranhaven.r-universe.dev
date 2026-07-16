#' Pre-solve target feasibility checks
#'
#' Runs two deterministic, closed-form feasibility checks before calibration.
#' Both rely only on the initial weight marginals that the solver preserves, so
#' they are cheap and exact within their stated scope.
#'
#' \enumerate{
#'   \item \strong{Overall-vs-group consistency.} Marginal totals are held fixed
#'     during calibration, so if every level of a grouping variable carries an
#'     exact target, the overall rate is uniquely pinned to
#'     \eqn{\sum_\ell W_\ell r_\ell / W}. Two such "complete" variables, or one
#'     plus an explicit overall target, can disagree; that disagreement is a
#'     guaranteed conflict under `mode = "exact"`.
#'   \item \strong{Single-target marginal interval.} With the group total fixed
#'     and per-unit multipliers bounded by `[lower, upper]`, a group's reachable
#'     weighted rate lies in a closed interval (two-block water-filling). A
#'     target outside that interval can never be met. This is a \emph{necessary}
#'     condition only: passing every single-target check does not guarantee the
#'     targets are jointly feasible, because overlapping units couple the groups.
#' }
#'
#' @param data A data frame with one row per sampled unit.
#' @param outcome Name of the binary outcome column (1 = pass, 0 = fail).
#' @param weight Name of the initial weight column.
#' @param group_vars Character vector of grouping-variable names.
#' @param targets Target table with columns `variable`, `level`, `target_rate`.
#' @param lower,upper Lower and upper bounds on the weight-adjustment multiplier.
#' @param tol Numeric tolerance for the consistency comparison.
#' @param x A `ratecalib_feasibility` object (for the print method).
#' @param ... Further arguments (ignored by the print method).
#'
#' @return A list of class `ratecalib_feasibility` with elements `consistency`
#'   (a list with `pins`, `consistent` and `detail`), `marginal` (a data frame
#'   of per-target achievable intervals), `necessary_ok` (logical) and `note`.
#' @examples
#' d <- example_rate_data(300)
#' targets <- make_rate_targets(overall = 0.62,
#'                              groups = list(sex = c(M = 0.66, F = 0.60)))
#' calibration_feasibility(d, "qualified", "initial_weight", "sex", targets)
#' @export
calibration_feasibility <- function(data, outcome, weight, group_vars,
                                    targets, lower = 0.25, upper = 4,
                                    tol = 1e-8) {
  if (!is.data.frame(data)) stop("data must be a data frame.", call. = FALSE)
  required <- unique(c(outcome, weight, group_vars))
  missing <- setdiff(required, names(data))
  if (length(missing)) {
    stop("Missing columns: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  targets <- as.data.frame(targets, stringsAsFactors = FALSE)
  if (!all(c("variable", "level", "target_rate") %in% names(targets))) {
    stop("targets must contain the columns variable, level and target_rate.",
         call. = FALSE)
  }

  w <- as.numeric(data[[weight]])
  y_raw <- data[[outcome]]
  if (is.logical(y_raw)) y_raw <- as.integer(y_raw)
  y <- as.numeric(as.character(y_raw))
  grand_total <- sum(w)

  is_overall <- function(v) v %in% c(".overall", "overall", "TOTAL")

  # This precheck only reasons about simple proportion-of-outcome targets on a
  # single dimension. Interaction (colon) keys, mean/total targets and
  # proportions of a non-outcome value are skipped (not misjudged).
  col_or_na <- function(nm) {
    if (nm %in% names(targets)) as.character(targets[[nm]])
    else rep(NA_character_, nrow(targets))
  }
  stat <- col_or_na("statistic"); stat[is.na(stat) | stat == ""] <- "proportion"
  vv <- col_or_na("value_var"); val <- col_or_na("value")
  analyzable <- stat == "proportion" &
    (is.na(vv) | vv == "" | vv == outcome) &
    (is.na(val) | val == "1") &
    !grepl(":", as.character(targets$variable), fixed = TRUE)

  # Per-(variable, level) initial pass / fail / total weight, and the achievable
  # rate interval given fixed group total and multiplier bounds.
  block <- function(mask) {
    P <- sum(w[mask & y == 1])
    Fl <- sum(w[mask & y == 0])
    W <- P + Fl
    max_pass <- min(upper * P, W - lower * Fl)
    min_pass <- max(lower * P, W - upper * Fl)
    list(P = P, F = Fl, W = W,
         min_rate = if (W > 0) min_pass / W else NA_real_,
         max_rate = if (W > 0) max_pass / W else NA_real_)
  }

  # --- check 2: single-target marginal intervals --------------------------
  marginal <- data.frame(
    variable = character(), level = character(), target_rate = numeric(),
    group_total = numeric(), min_achievable = numeric(),
    max_achievable = numeric(), within_range = logical(),
    stringsAsFactors = FALSE
  )
  for (i in which(analyzable)) {
    v <- as.character(targets$variable[i])
    lev <- as.character(targets$level[i])
    r <- as.numeric(targets$target_rate[i])
    mask <- if (is_overall(v)) rep(TRUE, nrow(data)) else as.character(data[[v]]) == lev
    b <- block(mask)
    within <- !is.na(b$min_rate) &&
      r >= b$min_rate - tol && r <= b$max_rate + tol
    marginal <- rbind(marginal, data.frame(
      variable = v, level = lev, target_rate = r, group_total = b$W,
      min_achievable = b$min_rate, max_achievable = b$max_rate,
      within_range = within, stringsAsFactors = FALSE
    ))
  }

  # --- check 1: overall-vs-group consistency ------------------------------
  pins <- data.frame(source = character(), implied_overall = numeric(),
                     stringsAsFactors = FALSE)

  # explicit overall target, if any (only simple proportion-of-outcome ones)
  ov <- targets[analyzable & vapply(targets$variable, is_overall, logical(1)), ,
                drop = FALSE]
  if (nrow(ov)) {
    pins <- rbind(pins, data.frame(source = ".overall",
                                   implied_overall = as.numeric(ov$target_rate[1]),
                                   stringsAsFactors = FALSE))
  }

  # a grouping variable pins the overall rate only if every data level is targeted
  for (v in group_vars) {
    sub <- targets[analyzable & as.character(targets$variable) == v, , drop = FALSE]
    if (!nrow(sub)) next
    data_levels <- unique(as.character(data[[v]]))
    if (!all(data_levels %in% as.character(sub$level))) next  # incomplete
    sub <- sub[match(data_levels, as.character(sub$level)), , drop = FALSE]
    Wl <- vapply(data_levels, function(l) sum(w[as.character(data[[v]]) == l]),
                 numeric(1))
    implied <- sum(Wl * as.numeric(sub$target_rate)) / grand_total
    pins <- rbind(pins, data.frame(source = v, implied_overall = implied,
                                   stringsAsFactors = FALSE))
  }

  consistent <- TRUE
  detail <- "no two sources pin the overall rate"
  if (nrow(pins) >= 2) {
    spread <- max(pins$implied_overall) - min(pins$implied_overall)
    consistent <- spread <= tol
    detail <- if (consistent) {
      sprintf("all %d sources agree on overall rate %.6g",
              nrow(pins), pins$implied_overall[1])
    } else {
      sprintf("sources disagree on the overall rate (range %.6g to %.6g)",
              min(pins$implied_overall), max(pins$implied_overall))
    }
  }

  necessary_ok <- consistent && all(marginal$within_range)

  note <- c(
    "Single-target intervals are a NECESSARY condition only: passing every target does not guarantee the targets are jointly feasible, because overlapping units couple the groups.",
    "The consistency identity holds because calibration preserves the initial weight marginals; a conflict is a hard infeasibility under mode='exact' and an unachievable combination under mode='soft'.",
    "For exact feasibility, the final word is the solver."
  )
  if (any(!analyzable)) {
    note <- c(note, sprintf(
      "%d target(s) are NOT analysed by this precheck (interaction keys, mean/total, or proportions of a non-outcome value).",
      sum(!analyzable)))
  }

  structure(list(
    consistency = list(pins = pins, consistent = consistent, detail = detail),
    marginal = marginal,
    necessary_ok = necessary_ok,
    note = note
  ), class = "ratecalib_feasibility")
}

#' @rdname calibration_feasibility
#' @export
print.ratecalib_feasibility <- function(x, ...) {
  cat("ratecalib feasibility precheck\n")
  cat("Necessary feasibility: ", if (isTRUE(x$necessary_ok)) "ok" else "FAILED",
      "\n", sep = "")
  cat("\nOverall consistency: ",
      if (isTRUE(x$consistency$consistent)) "consistent" else "INCONSISTENT",
      "\n  ", x$consistency$detail, "\n", sep = "")
  if (nrow(x$consistency$pins)) {
    cat("\nSources pinning the overall rate:\n")
    print(x$consistency$pins, row.names = FALSE)
  }
  cat("\nPer-target achievable intervals:\n")
  print(x$marginal, row.names = FALSE)
  cat("\nNote:\n", paste0("- ", x$note, collapse = "\n"), "\n", sep = "")
  invisible(x)
}
