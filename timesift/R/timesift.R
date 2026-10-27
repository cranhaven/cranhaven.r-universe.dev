#' Fit and compare representations of time-varying data
#'
#' One call from two tables to a scored comparison and a held-out estimate of choosing among it.
#' `targets` is one row per thing to predict and `series` is the long, time-stamped record belonging
#' to those rows. Every representation in `sift` is built and every learner in `models` is paired
#' with the ones it can read; each pair is a candidate.
#'
#' Within each outer fold of `resampling` the training targets are split again into `inner` folds.
#' Every candidate is cross-validated on that inner split, the rule picks one on its inner score,
#' and the stack's weights are fitted on the inner out-of-fold predictions. Every candidate is then
#' refitted on the whole outer training set and predicts the outer test fold, and the selected
#' candidate's prediction and the prediction combined under that fold's weights are kept. Nothing
#' the outer test fold holds enters the choice or the weights it is scored under, so `estimate` is
#' of the procedure, selection and stacking included, which is what an ecologist applying it to a
#' new site would run.
#'
#' The same refits give every candidate an out-of-fold prediction on the outer folds, and `scores`
#' holds those. They say where predictive skill saturates as the record is read more coarsely, which
#' is the measurement the package exists for, but the highest of them is a number the held-out
#' targets helped choose: read the candidates for the shape of the comparison and `estimate` for
#' the level. With `inner = NULL` no inner search is run, the candidates are compared on the outer
#' folds alone and no estimate is made.
#'
#' The cost is `v_outer * (v_inner + 1) * candidates` fits for the evaluation and one refit per
#' candidate on every target, against `v_outer * candidates` for the comparison alone.
#'
#' @param targets A data frame, one row per prediction target.
#' @param series A long data frame of readings, or `NULL` to fit on `static` alone.
#' @param y Columns of `targets` holding the response, as a tidyselect expression such as
#'   `starts_with("sp_")`.
#' @param x Columns of `series` holding the readings, as a tidyselect expression. Defaults to
#'   every numeric column but `id` and `time`.
#' @param id Column naming the unit, present in both tables. A bare column name or a string.
#' @param time Column of reading instants in `series`, `POSIXct`.
#' @param target_time Column of `targets` anchoring each row in time, `POSIXct`. Optional, and
#'   what a unit carrying several targets through time needs.
#' @param static Columns of `targets` carried alongside the representation, as a tidyselect
#'   expression. None by default.
#' @param models A learner, a set of them from [c()], or a list. Defaults to [elasticnet()].
#' @param sift The representations a learner without a `data =` of its own is run across.
#'   A [grains()] or [lookbacks()] set, a set from [c()], a bare vector of grain names, a single
#'   representation, or a list of them. Defaults to `grains("auto")`.
#' @param ensemble `TRUE` for the default stack, `FALSE` for none, or an [ensemble()] spec.
#' @param resampling The outer split: [cv()], [grouped_cv()], a fold vector, or a [fold_map()]
#'   result.
#' @param inner Number of inner folds the choice and the stack's weights are made on inside each
#'   outer training set, a function of the outer training response returning a fold map for those
#'   targets, or `NULL` to compare the candidates on the outer folds without an estimate. A count
#'   deals the inner folds by the grouping the outer split carries, as [select_grain()] does.
#' @param rule How a candidate is chosen from its inner scores, `"argmax"` or
#'   `"coarsest_adequate"`, as in [select_grain()].
#' @param response Name of the registered response head.
#' @param metric Name of a registered metric, or a function of `(y, p)`, or `NULL` for the
#'   response head's own. It is what the candidates are chosen on and what the report reads.
#'   Whichever it is, it travels with the fit and is what every later rescoring reads; a function is
#'   reported as `<function>`. The estimate is also reported under every registered metric.
#' @param control [train_control()], the training settings every neural learner reads.
#' @param keep_fits Keep every per-fold fitted candidate beside the refits.
#' @param seed Seed for the inner splits. Each outer fold splits under `seed` plus its position.
#' @param verbose Report each outer fold as it runs.
#'
#' @section Rules the entry point enforces:
#' `static` is never implicit: a column of `targets` that is neither the response, the identifier
#' nor the anchor is ignored unless `static` names it, because a predictor nobody asked for is
#' worse than one that is missing.
#'
#' One target row per `id`, unless `target_time` says where in time each row sits. Repeated
#' identifiers without an anchor are an error naming them.
#'
#' With `target_time`, every representation has to be anchored on the target, so [native()],
#' [grain()] and [multigrain()] are refused and `sift` must be given as [lookbacks()]. There is no
#' default set of spans, because there is no defensible one.
#'
#' Without `series`, `static` is the whole predictor block and `sift` is ignored.
#'
#' @section What a learner may be handed:
#' A learner declares whether it reads a tabular block or a sequence. A tabular learner given
#' [native()] is refused before anything is built, and a sequence learner given a representation of
#' one bin is refused once the array says how many bins it has. Inside a `sift` expansion such a
#' pair is skipped and reported once by name; named explicitly through a learner's `data =` it is
#' an error.
#'
#' @return A `timesift` object, a list carrying:
#'   * `estimate`: the held-out score of the selected candidate (`arm = "selected"`) and of the
#'     stack (`arm = "ensemble"`), one row per metric, with the standard error and the 95% interval
#'     across response variables. An interval across the variables of this dataset, not one for a
#'     new sample: every variable is fitted and scored on the same targets and folds, so the error
#'     they share is not in it. `NULL` with `inner = NULL`.
#'   * `selected`: one row per outer fold, the candidate it chose, the inner score it chose on, the
#'     highest inner score and that score's standard error; `inner`: every candidate's inner score
#'     in every outer fold; `fold_weights`: the stack's weights in every outer fold, one row per
#'     fold. All `NULL` with `inner = NULL`.
#'   * `predictions`: the held-out prediction of every target under the selected candidate and under
#'     the stack.
#'   * `candidates`, `scores` and `oof`: every candidate, its per-cell scores on the outer folds and
#'     its outer out-of-fold predictions.
#'   * `choice`, `models`, `stack` and `weights`: the procedure applied to every target, which is
#'     what [predict()] uses. Every candidate is refitted on all of them; `choice` is the candidate
#'     the rule takes on the outer scores, with the outer folds as the split it chooses on, and
#'     `stack` holds weights fitted on the outer out-of-fold predictions.
#'   * `representations`, `fits`, `folds`, `cells`, `y`, and the `metric`, `response`, `spec` and
#'     `call` it was asked for.
#'
#' @seealso [build_representation()] for the array a candidate reads, [fold_map()] for the splits.
#'
#' @examples
#' set.seed(1)
#' t <- seq(as.POSIXct("2021-09-01", tz = "UTC"), by = "hour", length.out = 24 * 90)
#' units <- sprintf("p%02d", 1:30)
#' warmth <- rnorm(30)
#' logger <- data.frame(
#'   plot = rep(units, each = length(t)), datetime = rep(t, 30),
#'   temp = as.numeric(vapply(warmth, function(w) w + sin(seq_along(t) / 300), numeric(length(t)))))
#' plots <- data.frame(plot = units,
#'                     sp_a = rbinom(30, 1, plogis(2 * warmth)),
#'                     sp_b = rbinom(30, 1, plogis(-2 * warmth)))
#' \donttest{
#' fit <- timesift(plots, logger, y = starts_with("sp_"), id = plot, time = datetime,
#'                 sift = grains("week", "month"), resampling = cv(v = 3L),
#'                 ensemble = FALSE, verbose = FALSE)
#' fit
#' }
#'
#' @export
timesift <- function(targets, series = NULL, y, x = NULL, id = NULL, time = NULL,
                     target_time = NULL, static = NULL,
                     models = NULL, sift = NULL, ensemble = TRUE,
                     resampling = cv(), inner = 5L, rule = c("argmax", "coarsest_adequate"),
                     response = "presence_absence", metric = NULL,
                     control = train_control(), keep_fits = FALSE, seed = 1L, verbose = TRUE) {
  call <- match.call()
  rule <- match.arg(rule)
  env <- parent.frame()
  if (!is.data.frame(targets)) {
    stop("`targets` must be a data frame, one row per prediction target, got ",
         class(targets)[1L], ".", call. = FALSE)
  }
  if (!is.null(series) && !is.data.frame(series)) {
    stop("`series` must be a long data frame of readings, or NULL, got ", class(series)[1L], ".",
         call. = FALSE)
  }

  id_col <- .optional_column(substitute(id), targets, env)
  target_time_col <- .optional_column(substitute(target_time), targets, env)
  time_col <- if (is.null(series)) NULL else .optional_column(substitute(time), series, env)
  y_cols <- .select_columns(rlang::enquo(y), targets, "y")
  static_cols <- .select_columns(rlang::enquo(static), targets, "static")

  if (!length(y_cols)) {
    stop("`y` must name the response column", if (ncol(targets) > 1L) "(s)" else "",
         " in `targets`.", call. = FALSE)
  }
  clash <- intersect(static_cols, c(y_cols, id_col, target_time_col))
  if (length(clash)) {
    stop("`static` names ", .listing(clash), ", which the response, the identifier or the anchor ",
         "already carries. A column is a predictor or it is one of those, not both.",
         call. = FALSE)
  }
  if (!is.null(series)) {
    if (is.null(id_col)) {
      stop("`id` must name the column linking `targets` to `series`.", call. = FALSE)
    }
    if (is.null(time_col)) {
      stop("`time` must name the column of reading instants in `series`.", call. = FALSE)
    }
    if (!id_col %in% names(series)) {
      stop("`", id_col, "` is in `targets` but not in `series`, so no target reaches a record.",
           call. = FALSE)
    }
  } else if (!length(static_cols)) {
    stop("without `series` there is nothing to fit on unless `static` names predictors in ",
         "`targets`.", call. = FALSE)
  }
  if (!is.null(target_time_col) && !inherits(targets[[target_time_col]], "POSIXct")) {
    stop("`", target_time_col, "` must be POSIXct, not ", class(targets[[target_time_col]])[1L],
         ".", call. = FALSE)
  }

  value_cols <- .series_values(rlang::enquo(x), series, id_col, time_col)
  spec <- list(id = id_col, time = time_col, value = value_cols, target_time = target_time_col,
               static = static_cols, y = y_cols, partial = "keep", response = response,
               control = control, sift = NULL)
  tf <- .target_frame(targets, spec)
  targets <- targets[tf$order, , drop = FALSE]
  .check_targets_unique(tf, spec)
  if (!is.null(series)) {
    .check_series_reaches(tf, series, spec)
  }

  head <- .responses_reg$get(response)
  y_matrix <- head$prepare(.response_block(targets, tf, y_cols))
  # Refused here rather than after the fitting, because a contradiction between the run's head and
  # the combiner's is not worth a grid of fits to find out about.
  if (!isFALSE(ensemble)) {
    ensemble <- .run_ensemble(ensemble, response)
  }

  learners <- .learner_list(models %||% list(elasticnet()))
  for (ln in names(learners)) .learner_contract(learners[[ln]], ln)
  .refuse_pinned(learners)
  if (is.null(series) && !is.null(sift) && verbose) {
    message("without `series` the predictor block is `static`, so `sift` is ignored.")
  }
  sift <- .sift_specs(sift, series)
  .check_anchored(sift, learners, spec)

  store <- .build_sift(sift, series, targets, spec, learners, verbose)
  spec$sift <- store$sift
  built <- timesift_set(store$built)
  grid <- .candidate_grid(learners, store$sift, built, store$labels)
  fitted <- grid[grid$status == "fitted", , drop = FALSE]
  if (!nrow(fitted)) {
    stop("no learner can read any representation in the sift:\n  ",
         paste(grid$note, collapse = "\n  "), call. = FALSE)
  }
  if (verbose && any(grid$status != "fitted")) {
    message("skipping ", .plural(sum(grid$status != "fitted"), "candidate"),
            " no learner can read: ", .listing(grid$candidate[grid$status != "fitted"]))
  }

  folds <- .as_fold_map(resampling, y_matrix, targets, tf)
  f <- .as_folds(folds, tf$label)
  group <- .fold_group(folds, tf$label)
  cells <- head$cells(y_matrix, folds)
  if (!any(cells$scorable)) {
    stop("no (response, fold) cell is scorable under this fold map, so no candidate can be ",
         "scored: every cell needs both classes on each side of its split. See scorable_cells() ",
         "for the counts; a rarer response needs fewer folds, or a response present somewhere.",
         call. = FALSE)
  }
  metric_arg <- metric
  metric <- .as_metric(metric, head$metric)
  score <- metric$fn
  levels <- sort(unique(f))
  nested <- !is.null(inner)
  stacking <- !isFALSE(ensemble) && nrow(fitted) >= 2L
  if (!isFALSE(ensemble) && !stacking && verbose) {
    message("no ensemble: stacking needs at least two candidates.")
  }
  if (nested && nrow(fitted) < 2L && verbose) {
    message("one candidate, so there is nothing to choose between inside the training folds.")
  }

  # The candidate set as the selection engine reads it: `grain` names the representation array, and
  # the order the candidates were declared in is both the fitting order and the order a tie falls in.
  pairs <- data.frame(grain = fitted$representation, learner = fitted$learner,
                      stringsAsFactors = FALSE)
  ctx <- .selection_context(built, y_matrix, learners, pairs, pairs, rule,
                            if (nested) .inner_splitter(inner, group) else NULL,
                            response, metric_arg %||% head$metric, control, group)
  n_cand <- nrow(fitted)
  blank <- matrix(NA_real_, nrow = nrow(y_matrix), ncol = ncol(y_matrix),
                  dimnames = dimnames(y_matrix))
  oof <- stats::setNames(rep(list(blank), n_cand), fitted$candidate)
  fits <- stats::setNames(rep(list(list()), n_cand), fitted$candidate)
  p_selected <- p_ensemble <- blank
  chosen <- inner_rows <- fold_weights <- vector("list", length(levels))

  for (i in seq_along(levels)) {
    k <- levels[i]
    started <- Sys.time()
    train <- which(f != k)
    test <- which(f == k)
    search <- if (nested && n_cand >= 2L) .inner_search(ctx, train, seed + i, fold = k) else NULL
    refit <- .refit_candidates(ctx, seq_len(n_cand), train, test)
    for (j in seq_len(n_cand)) {
      held <- refit$preds[[j]]
      oof[[j]][rownames(held), colnames(held)] <- held
      if (keep_fits) {
        fits[[j]][[as.character(k)]] <- refit$fits[[j]]
      }
    }
    if (nested) {
      won <- search$won %||% 1L
      held <- refit$preds[[won]]
      p_selected[rownames(held), colnames(held)] <- held
      chosen[[i]] <- .fold_choice(k, fitted[won, ], search, length(train), length(test))
      if (!is.null(search)) {
        inner_rows[[i]] <- .fold_inner(search$grid, fitted)
      }
      if (stacking) {
        st <- .fold_stack(search$lad, fitted, y_matrix[train, , drop = FALSE], ensemble)
        combined <- ensemble_combine(st, stats::setNames(refit$preds, fitted$candidate))
        p_ensemble[rownames(combined), colnames(combined)] <- combined
        fold_weights[[i]] <- as.data.frame(as.list(c(fold = k, st$weights)), check.names = FALSE)
      }
    }
    if (verbose) {
      message(sprintf("fold %s of %d%s, %.0f s", k, length(levels),
                      if (nested) paste0(" selected ", fitted$candidate[search$won %||% 1L]) else "",
                      as.numeric(difftime(Sys.time(), started, units = "secs"))))
    }
  }

  scores <- do.call(rbind, lapply(seq_len(n_cand), function(j) {
    .candidate_scores(fitted$candidate[j], fitted$representation[j], fitted$learner[j],
                      y_matrix, oof[[j]], f, levels, cells, score)
  }))
  rownames(scores) <- NULL

  if (verbose) {
    message("refitting every candidate on all ", nrow(y_matrix), " targets")
  }
  models_out <- stats::setNames(lapply(seq_len(n_cand), function(j) {
    fit_learner(learners[[fitted$learner[j]]], built[[fitted$representation[j]]], y_matrix,
                response = response, control = control, group = group)
  }), fitted$candidate)
  stack <- if (stacking) {
    ensemble_fit(oof = oof, y = y_matrix, cells = cells, folds = folds, spec = ensemble,
                 scores = scores)
  }

  estimate <- selected <- inner_table <- weights_table <- predictions <- NULL
  if (nested) {
    predictions <- list(selected = p_selected)
    estimate <- .run_estimate("selected", y_matrix, p_selected, f, levels, cells, metric)
    if (stacking) {
      predictions$ensemble <- p_ensemble
      estimate <- rbind(estimate,
                        .run_estimate("ensemble", y_matrix, p_ensemble, f, levels, cells, metric))
      weights_table <- do.call(rbind, fold_weights)
    }
    selected <- do.call(rbind, chosen)
    inner_table <- if (n_cand >= 2L) do.call(rbind, inner_rows)
  }

  structure(list(estimate = estimate, selected = selected, inner = inner_table,
                 fold_weights = weights_table, predictions = predictions,
                 candidates = grid, scores = scores, oof = oof,
                 choice = .run_choice(scores, fitted, ctx), models = models_out,
                 stack = stack, weights = stack$weights, representations = built,
                 fits = if (keep_fits) fits else NULL,
                 folds = folds, cells = cells, y = y_matrix,
                 metric = metric$name, scorer = score, response = response, spec = spec,
                 call = call),
            class = "timesift")
}

# ---- the nested evaluation -----------------------------------------------------------------------

.fold_choice <- function(k, row, search, n_train, n_test) {
  won <- search$won
  data.frame(fold = k, candidate = row$candidate, representation = row$representation,
             learner = row$learner,
             inner_score = if (is.null(won)) NA_real_ else search$grid$score[won],
             inner_best = if (is.null(won)) NA_real_ else search$grid$score[search$best],
             inner_se = if (is.null(won)) NA_real_ else search$grid$se[search$best],
             n_train = n_train, n_test = n_test, stringsAsFactors = FALSE)
}

.fold_inner <- function(grid, fitted) {
  data.frame(fold = grid$fold, candidate = fitted$candidate, representation = grid$grain,
             learner = grid$learner, score = grid$score, se = grid$se,
             n_variable = grid$n_variable, stringsAsFactors = FALSE)
}

# The weights one outer fold is combined under, fitted on the inner out-of-fold predictions of its
# own training targets over the inner split's scorable cells. The combiner never sees a prediction
# for a target of the outer test fold, nor that target's response.
.fold_stack <- function(lad, fitted, y_train, spec) {
  arms <- paste(fitted$representation, fitted$learner, sep = "|")
  inner_oof <- stats::setNames(attr(lad, "predictions")[arms], fitted$candidate)
  rows <- lad[!is.na(match(paste(lad$grain, lad$learner, sep = "|"), arms)), , drop = FALSE]
  inner_scores <- data.frame(candidate = paste(rows$learner, rows$grain, sep = " / "),
                             variable = rows$variable, fold = rows$fold, score = rows$score,
                             scorable = rows$scorable, stringsAsFactors = FALSE)
  ensemble_fit(oof = inner_oof, y = y_train[rownames(inner_oof[[1L]]), , drop = FALSE],
               cells = attr(lad, "cells"), folds = attr(lad, "folds"), spec = spec,
               scores = inner_scores)
}

# One arm of the estimate under every registered metric, and under the run's own where that is a
# function no registry holds, so the number the choice was made on is always one of the rows.
.run_estimate <- function(arm, y, p, f, levels, cells, metric) {
  out <- .nested_estimate(y, p, f, levels, cells)
  if (!metric$name %in% out$metric) {
    out <- rbind(out, .estimate_row(metric$name, .score_arm(arm, arm, y, p, f, levels, cells,
                                                            metric$fn)))
  }
  cbind(arm = arm, out, stringsAsFactors = FALSE)
}

# The procedure applied to every target: the rule, read on the outer scores with the outer folds as
# the split it chooses on. One candidate is its own choice.
.run_choice <- function(scores, fitted, ctx) {
  if (nrow(fitted) < 2L) {
    return(fitted$candidate[1L])
  }
  lad <- data.frame(grain = scores$representation, learner = scores$learner,
                    variable = scores$variable, fold = scores$fold, score = scores$score,
                    scorable = scores$scorable, stringsAsFactors = FALSE)
  grid <- .join_candidates(ctx$candidates, summary.timesift_ladder(lad), .inner_se(lad),
                           NA_integer_)
  fitted$candidate[.choose_candidate(grid, ctx$size, ctx$rule)]
}

# The value columns of the series. A caller who names them is held to them; a caller who does not
# gets the numeric ones, because the default cannot know that a text column is a note rather than
# a reading and an explicit choice is a decision rather than a guess.
.series_values <- function(quo, series, id_col, time_col) {
  if (is.null(series)) {
    return(character())
  }
  chosen <- .select_columns(quo, series, "x")
  if (!length(chosen)) {
    rest <- setdiff(names(series), c(id_col, time_col))
    chosen <- rest[vapply(series[rest], is.numeric, logical(1L))]
    if (!length(chosen)) {
      stop("`series` carries no numeric column beside `", id_col, "` and `", time_col,
           "`. Name the readings with `x`.", call. = FALSE)
    }
    return(chosen)
  }
  clash <- intersect(chosen, c(id_col, time_col))
  if (length(clash)) {
    stop("`x` names ", .listing(clash), ", which is the identifier or the time column.",
         call. = FALSE)
  }
  bad <- chosen[!vapply(series[chosen], is.numeric, logical(1L))]
  if (length(bad)) {
    stop("`x` names ", .listing(bad), ", which ", if (length(bad) > 1L) "are" else "is",
         " not numeric.", call. = FALSE)
  }
  chosen
}

.check_targets_unique <- function(tf, spec) {
  if (is.null(tf$id) || !is.null(spec$target_time)) {
    return(invisible(TRUE))
  }
  dup <- unique(tf$id[duplicated(tf$id)])
  if (length(dup)) {
    stop("`targets` holds more than one row for ", .plural(length(dup), "identifier"), ": ",
         .listing(dup), ". Give `target_time` to say where in time each row sits.", call. = FALSE)
  }
  invisible(TRUE)
}

.check_series_reaches <- function(tf, series, spec) {
  missing <- setdiff(unique(tf$id), unique(.unit_names(series[[spec$id]], spec$id)))
  if (length(missing)) {
    stop(.plural(length(missing), "target"), " name a unit `series` does not carry: ",
         .listing(missing), ".", call. = FALSE)
  }
  invisible(TRUE)
}

.response_block <- function(targets, tf, y_cols) {
  out <- as.matrix(targets[, y_cols, drop = FALSE])
  storage.mode(out) <- "double"
  dimnames(out) <- list(tf$label, y_cols)
  out
}

# A learner declares what it can be handed and whether one fitted model covers every response.
# `reads` decides what the fitting layer may pair it with and `multi` is what a report says of the
# candidate, so neither has a default to fall back on.
.learner_contract <- function(learner, label) {
  if (!isTRUE(learner$reads %in% c("tabular", "sequence"))) {
    stop("the ", label, " learner does not declare `reads`, which is \"tabular\" or \"sequence\".",
         call. = FALSE)
  }
  if (!isTRUE(learner$multi %in% c("joint", "separate"))) {
    stop("the ", label, " learner does not declare `multi`, which is \"joint\" or \"separate\".",
         call. = FALSE)
  }
  if (!is.null(learner$data) && !inherits(learner$data, "timesift_representation")) {
    stop("the ", label, " learner's `data` must be a representation or NULL, got ",
         class(learner$data)[1L], ".", call. = FALSE)
  }
  invisible(TRUE)
}

# The sift as representations, before any record is read: without a series there is one block and
# it is the static one, and the automatic set is still a promise the record has to keep.
.sift_specs <- function(sift, series) {
  if (is.null(series)) {
    return(as_sift(list(.static_representation())))
  }
  if (is.null(sift)) {
    return(grains("auto"))
  }
  as_sift(sift)
}

# Whether the targets are anchored in time and whether a representation is has to be one answer,
# and the check sees every representation the run will build: the members of the sift and the ones
# learners pinned themselves to through `data`. A lookback left without `target_time` reaches the
# builder with no anchor to place its bins against, which is a row count that does not add up
# rather than a message.
.check_anchored <- function(sift, learners, spec) {
  pinned <- Filter(Negate(is.null), lapply(learners, function(l) l$data))
  reps <- c(unclass(sift), pinned)
  if (is.null(spec$target_time)) {
    wrong <- Filter(function(r) identical(r$kind, "lookback"), reps)
    if (length(wrong)) {
      .needs_target_time(vapply(wrong, function(r) r$label, character(1L)))
    }
    return(invisible(TRUE))
  }
  if (isTRUE(attr(sift, "auto"))) {
    stop("`target_time` anchors every target in time, so every representation has to be a ",
         "lookback and there is no default set of spans. Give `sift = lookbacks(...)`.",
         call. = FALSE)
  }
  wrong <- Filter(function(r) !identical(r$kind, "lookback"), reps)
  if (length(wrong)) {
    labels <- vapply(wrong, function(r) r$label, character(1L))
    stop("`target_time` needs a target-anchored representation, and ", .listing(unique(labels)),
         " follows the calendar. Give `sift = lookbacks(...)`, or drop `target_time`.",
         call. = FALSE)
  }
  invisible(TRUE)
}

# Every array a candidate could read, built once and shared: two learners on one representation
# read one array, and the automatic set keeps the grains it built to decide with. A learner
# pinned to a representation of its own adds it to the same store under its own label.
.build_sift <- function(sift, series, targets, spec, learners, verbose) {
  if (isTRUE(attr(sift, "auto"))) {
    tf <- .target_frame(targets, spec)
    if (verbose) {
      message("choosing the grains the record carries")
    }
    built <- .auto_grains(attr(sift, "stats"), attr(sift, "year_start"), series, tf, spec)
    reps <- lapply(names(built), grain, stats = attr(sift, "stats"),
                   year_start = attr(sift, "year_start"))
    sift <- as_sift(stats::setNames(reps, names(built)))
  } else {
    built <- lapply(sift, build_representation, series = series, targets = targets, spec = spec)
    names(built) <- names(sift)
  }
  labels <- names(sift)
  known <- as.list(sift)
  for (ln in names(learners)) {
    rep <- learners[[ln]]$data
    if (is.null(rep)) {
      next
    }
    if (!is.null(known[[rep$label]])) {
      if (!identical(known[[rep$label]], rep)) {
        stop("two representations are reported under the name \"", rep$label,
             "\": the one the ", ln, " learner is pinned to and the one in the sift. Name the ",
             "sift to tell them apart.", call. = FALSE)
      }
      next
    }
    known[[rep$label]] <- rep
    built[[rep$label]] <- build_representation(rep, series, targets, spec)
  }
  list(built = built, sift = as_sift(known), labels = labels)
}

# One row per (learner, representation) pair considered, whether or not it was fitted, so what was
# skipped is reported rather than absent. A pair the caller asked for by name is an error instead:
# a representation named through `data =` is a decision, not one arm of an expansion.
.candidate_grid <- function(learners, sift, built, expansion) {
  rows <- list()
  for (ln in names(learners)) {
    learner <- learners[[ln]]
    pinned <- !is.null(learner$data)
    labels <- if (pinned) learner$data$label else expansion
    for (label in labels) {
      rep <- sift[[label]]
      x <- built[[label]]
      note <- .compatibility(learner, ln, rep, dim(x)[2L])
      if (pinned && !is.null(note)) {
        stop(note, call. = FALSE)
      }
      rows[[paste(ln, label)]] <- data.frame(
        candidate = paste(ln, label, sep = " / "), representation = label, learner = ln,
        grain = .grain_of(rep), bins = dim(x)[2L], channels = dim(x)[3L],
        status = if (is.null(note)) "fitted" else "not applicable",
        note = note %||% NA_character_, stringsAsFactors = FALSE)
    }
  }
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

.grain_of <- function(rep) {
  switch(rep$kind,
    grain = if (is.function(rep$grain)) "custom" else rep$grain,
    multigrain = if (is.null(rep$grains)) "multigrain" else paste(rep$grains, collapse = "+"),
    rep$kind)
}

# What a learner can be handed. The unreduced record is refused to a tabular learner from the
# representation alone, before any array exists; a sequence learner is refused a representation
# that declares itself a block of features the same way, and one whose record turned out to hold a
# single bin once `bins` is known, so the message names the representation rather than the setting.
.compatibility <- function(learner, label, rep, bins = NA_integer_) {
  if (identical(learner$reads, "tabular") && identical(rep$kind, "grain") &&
        identical(rep$grain, "native")) {
    return(paste0("`", label, "()` reads a tabular representation; `native()` gives it one column ",
                  "per reading. Use `grain()`, `multigrain()` or `lookback()`."))
  }
  one_bin <- !isTRUE(rep$sequence) || (!is.na(bins) && bins < 2L)
  if (identical(learner$reads, "sequence") && one_bin) {
    return(paste0("`", label, "()` reads a sequence; `", .constructor_of(rep),
                  "` gives one row of features."))
  }
  NULL
}

# A learner pinned to a representation it cannot read is refused before the record is touched,
# because building the array it would have been given is the expensive half of the call.
.refuse_pinned <- function(learners) {
  for (ln in names(learners)) {
    rep <- learners[[ln]]$data
    if (is.null(rep)) {
      next
    }
    note <- .compatibility(learners[[ln]], ln, rep)
    if (!is.null(note)) {
      stop(note, call. = FALSE)
    }
  }
  invisible(TRUE)
}

.constructor_of <- function(rep) {
  switch(rep$kind,
    grain = if (identical(rep$grain, "native")) "native()" else
      paste0("grain(\"", .grain_of(rep), "\")"),
    multigrain = "multigrain()",
    lookback = "lookback()",
    static = "static predictors alone")
}

# One candidate, over every fold, into the out-of-fold matrix the layers above read. Nothing above
# this loop knows whether the learner covered the responses jointly or one at a time.
.fit_candidate <- function(learner, x, y, f, levels, response, control, keep_fits,
                           verbose = FALSE, group = NULL) {
  p <- matrix(NA_real_, nrow = nrow(y), ncol = ncol(y), dimnames = dimnames(y))
  fits <- list()
  for (k in levels) {
    started <- Sys.time()
    train <- which(f != k)
    test <- which(f == k)
    fit <- fit_learner(learner, .subset_units(x, train), y[train, , drop = FALSE],
                       response = response, control = control, group = group[train])
    held_out <- stats::predict(fit, .subset_units(x, test))
    if (verbose) {
      message(sprintf("  fold %s of %d, %.0f s", k, length(levels),
                      as.numeric(difftime(Sys.time(), started, units = "secs"))))
    }
    p[rownames(held_out), colnames(held_out)] <- held_out
    if (keep_fits) {
      fits[[as.character(k)]] <- fit
    }
  }
  list(oof = p, fits = if (keep_fits) fits else NULL)
}

.candidate_scores <- function(candidate, representation, learner, y, p, f, levels, cells, score) {
  out <- .score_arm(representation, learner, y, p, f, levels, cells, score)
  cbind(candidate = candidate, out[c("representation", "learner", "variable", "fold", "score",
                                     "scorable")],
        stringsAsFactors = FALSE)
}

#' Predict from a fitted timesift
#'
#' Rebuilds every member's representation for the new targets from the settings its own arm was
#' built with, predicts with the model refitted on all targets, and combines them where the
#' ensemble is asked for.
#'
#' @param object A [timesift()] fit.
#' @param targets A data frame of targets, carrying the identifier, the anchor and the static
#'   columns the fit was given.
#' @param series The long table of readings for those targets, or `NULL` for a targets-only fit.
#' @param candidate `"ensemble"`, the stack refitted on every target; `"selected"`, the candidate
#'   the rule chose on every target (`object$choice`); or the name of one candidate.
#' @param ... Ignored.
#'
#' @return A `[target, response]` matrix of predictions, named by target and in the order the fit
#'   carries its own targets: sorted by identifier, or the targets' own order where `target_time`
#'   anchors them.
#'
#' @export
predict.timesift <- function(object, targets, series = NULL, candidate = "ensemble", ...) {
  spec <- object$spec
  if (identical(candidate, "ensemble")) {
    if (is.null(object$stack)) {
      stop("this fit carries no ensemble; name a candidate or \"selected\": ",
           .listing(names(object$models)), ".", call. = FALSE)
    }
    members <- names(object$stack$weights)
  } else if (identical(candidate, "selected")) {
    members <- object$choice
  } else {
    members <- candidate
  }
  unknown <- setdiff(members, names(object$models))
  if (length(unknown)) {
    stop("unknown candidate: ", .listing(unknown), ". This fit carries ",
         .listing(names(object$models)), ".", call. = FALSE)
  }
  # The new targets are held to what the fit's own were held to: one row per identifier unless
  # `target_time` places the rows in time. Two rows for one plot would predict twice, silently.
  .check_targets_unique(.target_frame(targets, spec), spec)
  labels <- object$candidates$representation[match(members, object$candidates$candidate)]
  built <- lapply(stats::setNames(unique(labels), unique(labels)), function(label) {
    build_representation(spec$sift[[label]], series, targets, spec)
  })
  preds <- stats::setNames(lapply(seq_along(members), function(i) {
    stats::predict(object$models[[members[i]]], built[[labels[i]]])
  }), members)
  if (identical(candidate, "ensemble")) {
    ensemble_combine(object$stack, preds)
  } else {
    preds[[1L]]
  }
}
