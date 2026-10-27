#' What a run found
#'
#' Two kinds of row. A candidate row is the candidate's mean score on the outer folds, how many
#' responses it scored highest on, and whether one fitted model covered those responses or one was
#' fitted per response. These rows are the comparison: they share folds and cells, so their shape
#' is read across grains and learners, but the highest of them was picked out on the folds it is
#' scored on. The `selected` and `ensemble` rows are the procedure's held-out score, the choice and
#' the weights made inside every outer training fold, and they are the level to quote.
#'
#' Both columns beside a candidate's mean are worth reading. A candidate can carry the ensemble
#' without winning a single response, which is what `won` shows and a mean alone hides; and a joint
#' model and a per-response one reach the same `[target, response]` matrix by different routes,
#' which is what `responses` records.
#'
#' A candidate the run built no representation for, because its learner cannot read the
#' representation it was paired with, is listed with no mean rather than dropped, so the report says
#' what was asked for as well as what ran.
#'
#' @param object A `timesift` result.
#' @param x A `timesift` result, or the table this returns.
#' @param ... Ignored, so that the methods take the arguments their generics declare.
#'
#' @return A data frame of class `timesift_summary`, one row per candidate and, where the run made
#'   an estimate, one for the selected candidate and one for the stack. It carries the mean score,
#'   its standard error across responses for the two procedure rows, the responses won, how the
#'   responses were covered, and `scored`: `"outer folds"` for a candidate, `"nested"` for the
#'   procedure. The weights fitted on every target are in the `weights` attribute and the candidate
#'   chosen on every target in `choice`.
#'
#' @name timesift_report
NULL

#' @rdname timesift_report
#' @export
summary.timesift <- function(object, ...) {
  per_response <- .per_response(object)
  candidates <- object$candidates$candidate
  mean_score <- vapply(candidates, function(cd) {
    v <- per_response$score[per_response$candidate == cd]
    if (!length(v)) NA_real_ else mean(v)
  }, numeric(1L))
  out <- data.frame(candidate = candidates, mean = as.numeric(mean_score), se = NA_real_,
                    won = .responses_won(per_response, candidates),
                    responses = .candidate_multi(object, candidates), scored = "outer folds",
                    stringsAsFactors = FALSE)
  out <- out[order(out$mean, na.last = FALSE, decreasing = FALSE), , drop = FALSE]
  est <- object$estimate
  if (!is.null(est)) {
    est <- est[est$metric == object$metric, , drop = FALSE]
    out <- rbind(out, data.frame(candidate = est$arm, mean = est$score, se = est$se,
                                 won = NA_integer_, responses = "", scored = "nested",
                                 stringsAsFactors = FALSE))
  }
  rownames(out) <- NULL
  structure(out, class = c("timesift_summary", "data.frame"),
            header = .run_header(object), weights = ensemble_weights(object),
            choice = object$choice, selected = .selection_counts(object$selected))
}

# The stack's held-out score under the run's own metric, or NA where the run made no estimate of it.
.ensemble_estimate <- function(fit) {
  est <- fit$estimate
  if (is.null(est)) {
    return(NA_real_)
  }
  hit <- est$score[est$arm == "ensemble" & est$metric == fit$metric]
  if (length(hit)) hit[1L] else NA_real_
}

# How often each candidate was chosen across the outer folds, most often first.
.selection_counts <- function(selected) {
  if (is.null(selected)) {
    return(NULL)
  }
  counts <- table(factor(selected$candidate, levels = unique(selected$candidate)))
  sort(counts, decreasing = TRUE)
}

#' @rdname timesift_report
#' @export
print.timesift <- function(x, ...) {
  print(summary(x))
  invisible(x)
}

#' @rdname timesift_report
#' @export
print.timesift_summary <- function(x, ...) {
  cat(attr(x, "header"), "\n\n", sep = "")
  if (!nrow(x)) {
    cat("no candidate scored a cell.\n")
    return(invisible(x))
  }
  width <- max(nchar(c(x$candidate, "candidate")))
  row <- function(...) cat(trimws(sprintf(...), which = "right"), "\n", sep = "")
  cand <- x[x$scored == "outer folds", , drop = FALSE]
  cat("candidates, scored on the outer folds\n")
  row("%-*s %14s %6s  %s", width, "candidate", "mean", "won", "responses")
  for (i in seq_len(nrow(cand))) {
    row("%-*s %14s %6s  %s", width, cand$candidate[i],
        if (is.na(cand$mean[i])) "not applicable" else sprintf("%.3f", cand$mean[i]),
        if (is.na(cand$won[i])) "-" else format(cand$won[i]),
        cand$responses[i])
  }
  proc <- x[x$scored == "nested", , drop = FALSE]
  if (nrow(proc)) {
    cat("\nprocedure, chosen and weighted inside each outer training fold\n")
    for (i in seq_len(nrow(proc))) {
      row("%-*s %14s  se %.3f", width, proc$candidate[i], sprintf("%.3f", proc$mean[i]),
          proc$se[i])
    }
    counts <- attr(x, "selected")
    if (!is.null(counts)) {
      cat("selected ", paste(sprintf("%s in %d", names(counts), as.integer(counts)),
                             collapse = ", "),
          " of ", sum(counts), " folds\n", sep = "")
    }
  }
  if (!is.null(attr(x, "choice"))) {
    cat("\nchoice on every target  ", attr(x, "choice"), "\n", sep = "")
  }
  weights <- attr(x, "weights")
  if (!is.null(weights)) {
    # A member whose weight rounds to nothing is not a member of the combination in any way a
    # reader can act on; ensemble_weights() still carries every one of them.
    weights <- sort(weights[weights >= 0.005], decreasing = TRUE)
    cat("weights on every target  ",
        paste(sprintf("%s %.2f", names(weights), weights), collapse = "   "), "\n", sep = "")
  }
  invisible(x)
}

#' @rdname occlusion
#'
#' @details
#' Reached through a [timesift()] run rather than through a ladder, the profile reads the per-fold
#' models the run was told to keep, so every bin is held back from a model that never saw the units
#' it is rescored on. The candidate is named as `summary()` reports it.
#'
#' @param candidate Name of the candidate to read, for a run.
#'
#' @export
occlusion.timesift <- function(x, candidate, over = c("bin", "channel"), ...) {
  over <- match.arg(over)
  fit <- x
  .check_run(fit)
  row <- .candidate_row(fit, candidate)
  arm <- paste(row$representation, row$learner, sep = "|")
  fits <- .fold_fits(fit, candidate, arm)
  occlusion(.ladder_view(fit, candidate, row, fits),
            fit$representations[row$representation], fit$y, arm, over = over, ...)
}

# ---- reading the fitted object ---------------------------------------------------------------

.check_run <- function(fit) {
  if (!inherits(fit, "timesift")) {
    stop("expected a timesift() result, got ", class(fit)[1L], ".", call. = FALSE)
  }
  invisible(TRUE)
}

.candidate_row <- function(fit, candidate) {
  i <- match(candidate, fit$candidates$candidate)
  if (length(candidate) != 1L || is.na(i)) {
    stop("no candidate called \"", paste(candidate, collapse = ", "), "\" in this run. It fitted ",
         paste(fit$candidates$candidate, collapse = ", "), ".", call. = FALSE)
  }
  fit$candidates[i, , drop = FALSE]
}

# A response is the independent replicate, so a candidate's cells are averaged within a response
# over its folds before anything is averaged across responses. It is the ladder's rule, on the
# columns a run reports under.
.per_response <- function(fit) {
  scores <- fit$scores
  keep <- scores[!is.na(scores$score) & scores$candidate != "ensemble", , drop = FALSE]
  empty <- data.frame(candidate = character(), variable = character(), score = numeric(),
                      representation = character(), learner = character(),
                      stringsAsFactors = FALSE)
  if (!nrow(keep)) {
    return(empty)
  }
  per <- .cell_means(keep, "candidate")
  i <- match(per$candidate, fit$candidates$candidate)
  per$representation <- fit$candidates$representation[i]
  per$learner <- fit$candidates$learner[i]
  # The order the run declared its candidates in, so which candidate an exact tie falls to does not
  # depend on the session's collation.
  per[order(match(per$candidate, fit$candidates$candidate), per$variable, method = "radix"),
      names(empty), drop = FALSE]
}

.responses_won <- function(per_response, candidates) {
  won <- stats::setNames(integer(length(candidates)), candidates)
  if (!nrow(per_response)) {
    return(unname(won))
  }
  for (v in unique(per_response$variable)) {
    rows <- per_response[per_response$variable == v, , drop = FALSE]
    top <- rows$candidate[which.max(rows$score)]
    won[[top]] <- won[[top]] + 1L
  }
  unname(won)
}

# Whether one fitted model covered every response is the learner's own declaration, read off the
# model the run refit on all targets rather than recorded a second time beside it.
.candidate_multi <- function(fit, candidates) {
  vapply(candidates, function(cd) {
    model <- fit$models[[cd]]
    if (is.null(model) || is.null(model$learner)) NA_character_ else model$learner$multi
  }, character(1L), USE.NAMES = FALSE)
}

.run_header <- function(fit) {
  v <- length(unique(.as_folds(fit$folds, rownames(fit$y))))
  kind <- if (isTRUE(attr(fit$folds, "grouped"))) "grouped" else "random"
  sprintf("timesift  %s, %s, %d-fold %s CV, %s", .plural(nrow(fit$y), "target"),
          .plural(ncol(fit$y), "response"), v, kind, fit$metric)
}

# The per-fold models the run kept, rekeyed onto the arm label the occlusion reads them under. A
# run that kept none says so rather than reading the model refit on every target, which would
# rescore each fold on units that model was fitted on.
.fold_fits <- function(fit, candidate, arm) {
  if (is.null(fit$fits) || !length(fit$fits)) {
    stop("this run kept no per-fold fits. Refit with timesift(..., keep_fits = TRUE).",
         call. = FALSE)
  }
  kept <- fit$fits[[candidate]]
  if (is.null(kept) || !length(kept) || is.null(names(kept))) {
    stop("no fold of \"", candidate, "\" is among the fits this run kept.", call. = FALSE)
  }
  stats::setNames(kept, paste(arm, names(kept), sep = "|"))
}

# One candidate of a run, in the layout a ladder carries, so occlusion() reads a run through
# exactly the reader it already has.
.ladder_view <- function(fit, candidate, row, fits) {
  rows <- fit$scores[fit$scores$candidate == candidate, , drop = FALSE]
  arm <- paste(row$representation, row$learner, sep = "|")
  out <- data.frame(grain = row$representation, learner = row$learner, variable = rows$variable,
                    fold = rows$fold, score = rows$score, scorable = rows$scorable,
                    stringsAsFactors = FALSE)
  structure(out, class = c("timesift_ladder", "data.frame"),
            predictions = stats::setNames(list(fit$oof[[candidate]]), arm),
            cells = fit$cells, folds = fit$folds, fits = fits,
            metric = fit$metric, scorer = fit$scorer, response = fit$response)
}
