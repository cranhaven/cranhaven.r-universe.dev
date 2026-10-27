#' Fit at every grain and see where skill saturates
#'
#' Cross-validates every learner at every grain of a representation set, on one fold map and one
#' mask of scorable cells, and returns the score of each `(grain, learner, variable, fold)` cell.
#' It is the measurement the package exists for: how much of a record a model needs, read off the
#' point where making the record finer stops paying.
#'
#' Every arm sees identical splits and is restricted to identical cells, so the arms' means share a
#' denominator and any two of them can be compared cell by cell with [paired_contrast()].
#'
#' @param x A [grain_matrix()] result, a [timesift_set()], or a named list of representations.
#' @param y The response for the same units.
#' @param learners A learner, a set of them from [c()], a list, or names of registered ones. An
#'   unnamed set is labelled by each learner's own name.
#' @param folds A fold map from [fold_map()], or any named integer vector. Built with the defaults
#'   of [fold_map()] when not given.
#' @param response Name of the registered response head.
#' @param metric Name of a registered metric, or a function of `(y, p)`, or `NULL` for the
#'   response head's own. Whichever it is, it travels with the fit and is what every later
#'   rescoring reads; a function is reported as `<function>`.
#' @param control [train_control()], the training settings every neural learner of the ladder
#'   reads. A learner carrying a control of its own overrides it on the settings that control
#'   names.
#' @param keep_fits Keep every per-fold fitted model, which is what lets [occlusion()] read a
#'   fitted model without refitting it.
#' @param interval `"nested_cv"` refits every arm inside every outer training set, which is what
#'   [paired_contrast()] reads an interval for the difference in risk off; see [select_grain()]
#'   for what it estimates and what it costs. `"variables"`, the default, fits nothing further,
#'   and a contrast is then read across the response variables.
#' @param repeats Repetitions of the nested cross-validation, each on its own fold map. The first
#'   is the map the ladder was cross-validated on.
#' @param seed Seed the repetitions' fold maps are drawn under. Two tables whose contrast is to be
#'   read take the same `folds`, `repeats` and `seed`.
#' @param verbose Report each arm and each fold as it runs.
#' @param ... Ignored, so that `summary()` takes the arguments its generic declares.
#'
#' @return A data frame of one row per scored cell, of class `timesift_ladder`, carrying the
#'   grain, the learner, the variable, the fold and the score. The held-out prediction of every
#'   unit is kept in the `predictions` attribute, and the scorable-cell mask in `cells`.
#'
#' @examples
#' set.seed(1)
#' t <- seq(as.POSIXct("2021-09-01", tz = "UTC"), by = "hour", length.out = 24 * 200)
#' units <- sprintf("p%02d", 1:60)
#' warmth <- rnorm(60)
#' d <- data.frame(
#'   plot = rep(units, each = length(t)), t = rep(t, length(units)),
#'   temp = as.numeric(vapply(warmth, function(w) w + sin(seq_along(t) / 300) + rnorm(length(t)),
#'                            numeric(length(t)))))
#' y <- matrix(rbinom(120, 1, plogis(c(warmth, -warmth))), nrow = 60,
#'             dimnames = list(units, c("sp1", "sp2")))
#' x <- grain_matrix(d, plot, t, temp, grain = c("week", "month"))
#' lad <- grain_ladder(x, y, elasticnet(), folds = fold_map(y, v = 3), verbose = FALSE)
#' summary(lad)
#'
#' @export
grain_ladder <- function(x, y, learners, folds = NULL, response = "presence_absence",
                          metric = NULL, control = train_control(), keep_fits = FALSE,
                          interval = c("variables", "nested_cv"), repeats = 1L, seed = 1L,
                          verbose = TRUE) {
  interval <- .check_interval(interval[1L])
  set <- .as_set(x)
  learners <- .learner_list(learners)
  .ladder(set, y, learners, .cross_pairs(set, learners), folds = folds, response = response,
          metric = metric, control = control, keep_fits = keep_fits, interval = interval,
          repeats = repeats, seed = seed, verbose = verbose)
}

# Every grain with every learner, grain by grain, which is the grid a ladder is asked for.
.cross_pairs <- function(set, learners) {
  data.frame(grain = rep(names(set), each = length(learners)),
             learner = rep(names(learners), times = length(set)), stringsAsFactors = FALSE)
}

# The ladder over an explicit list of (grain, learner) pairs. A ladder asks for the full cross of
# its grains and learners; a run asks for the pairs whose learner can read the representation, and
# the search inside every outer training set of a selection asks for whichever of the two its
# caller holds. All three go through this one fold loop and this one scoring rule.
.ladder <- function(set, y, learners, pairs, folds = NULL, response = "presence_absence",
                    metric = NULL, control = train_control(), keep_fits = FALSE,
                    interval = "variables", repeats = 1L, seed = 1L, verbose = TRUE) {
  units <- dimnames(set[[1L]])[[1L]]
  spec <- .responses_reg$get(response)
  y <- .align_response(spec$prepare(y), units)
  if (is.null(folds)) {
    folds <- fold_map(y)
  }
  f <- .as_folds(folds, units)
  group <- .fold_group(folds, units)
  cells <- spec$cells(y, stats::setNames(f, units))
  metric <- .as_metric(metric, spec$metric)
  score <- metric$fn

  levels <- sort(unique(f))
  rows <- list()
  preds <- list()
  fits <- list()
  for (i in seq_len(nrow(pairs))) {
    w <- pairs$grain[i]
    ln <- pairs$learner[i]
    arm <- paste(w, ln, sep = "|")
    if (verbose) {
      message("fitting ", ln, " at the ", w, " grain")
    }
    # One fold loop for the package: a learner declaring one model per response is fitted that
    # way whichever door it came in by, and the ladder and a whole run cannot drift apart on
    # what a declared field means.
    run <- .fit_candidate(learners[[ln]], set[[w]], y, f, levels, response, control = control,
                          keep_fits = keep_fits, verbose = verbose, group = group)
    preds[[arm]] <- run$oof
    for (k in names(run$fits)) {
      fits[[paste(arm, k, sep = "|")]] <- run$fits[[k]]
    }
    rows[[arm]] <- .as_grain_rows(.score_arm(w, ln, y, run$oof, f, levels, cells, score))
  }
  out <- do.call(rbind, rows)
  rownames(out) <- NULL

  # Nested cross-validation refits every arm inside every outer training set of every repetition,
  # and keeps the predictions paired_contrast() reads the interval off.
  ncv <- NULL
  if (interval == "nested_cv") {
    maps <- .ncv_maps(y, f, group, repeats, seed)
    kept <- list()
    for (i in seq_len(nrow(pairs))) {
      w <- pairs$grain[i]
      ln <- pairs$learner[i]
      arm <- paste(w, ln, sep = "|")
      if (verbose) {
        message("nested cross-validation of ", arm, ": ", length(maps), " repetition(s)")
      }
      runs <- .ncv_collect(function(train, test, tag) {
        fit <- fit_learner(learners[[ln]], .subset_units(set[[w]], train),
                           y[train, , drop = FALSE], response = response, control = control,
                           group = group[train])
        stats::predict(fit, .subset_units(set[[w]], test))
      }, y, maps, preds[[arm]])
      kept[[arm]] <- .ncv_keep(runs)
    }
    ncv <- .ncv_record(y, maps, kept)
  }
  structure(out, class = c("timesift_ladder", "data.frame"),
            predictions = preds, cells = cells, folds = stats::setNames(f, units),
            fits = if (keep_fits) fits else NULL,
            metric = metric$name, scorer = score, response = response, ncv = ncv)
}

# One arm's cells, scored. The label is a representation, which a calendar grain is one kind of,
# so the column is named for the general case and a grain ladder relabels it below.
.score_arm <- function(representation, learner, y, p, f, levels, cells, score, at = NULL) {
  cbind(representation = representation, learner = learner,
        .score_cells(y, p, f, levels, cells, score,
                     arm = paste(representation, learner, sep = "|"), at = at),
        stringsAsFactors = FALSE)
}

# `at`, where given, is a [fold, variable] matrix of one cut per cell, handed to the metric as its
# third argument: the cell is then read at a cut learned elsewhere rather than one chosen on it.
.score_cells <- function(y, p, f, levels, cells, score, arm = NULL, at = NULL) {
  grid <- expand.grid(variable = colnames(y), fold = levels,
                      KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
  key <- paste(grid$variable, grid$fold)
  ok <- cells$scorable[match(key, paste(cells$variable, cells$fold))]
  ok[is.na(ok)] <- FALSE
  .check_finite(p, f, grid, ok, arm)
  value <- rep(NA_real_, nrow(grid))
  for (i in which(ok)) {
    rows <- f == grid$fold[i]
    value[i] <- if (is.null(at)) {
      score(y[rows, grid$variable[i]], p[rows, grid$variable[i]])
    } else {
      score(y[rows, grid$variable[i]], p[rows, grid$variable[i]],
            at[as.character(grid$fold[i]), grid$variable[i]])
    }
  }
  data.frame(variable = grid$variable, fold = grid$fold, score = value, scorable = ok,
             stringsAsFactors = FALSE)
}

# A scorable cell is one every candidate has to carry a prediction for. A threshold metric returns
# NA on a cell holding a prediction that is not a number, which is the same NA a cell with no
# score in it returns, so a network whose training diverged would be reported as an unscorable
# cell and dropped from the combiner without a word. It is a fit that did not settle, and it stops
# the run where it happened.
.check_finite <- function(p, f, grid, ok, arm) {
  hit <- which(ok)
  bad <- hit[vapply(hit, function(i) {
    !all(is.finite(p[f == grid$fold[i], grid$variable[i]]))
  }, logical(1L))]
  if (!length(bad)) {
    return(invisible(TRUE))
  }
  stop(if (is.null(arm)) "the predictions hold" else paste0("the ", arm, " arm holds"),
       " no number on ", .plural(length(bad), "scorable cell"), ", first ",
       grid$variable[bad[1L]], " in fold ", grid$fold[bad[1L]],
       ". A scorable cell is one every candidate carries, so this is a fit that did not settle ",
       "rather than a cell with no score in it.", call. = FALSE)
}

#' Score held-out predictions on the cells the mask allows
#'
#' The scoring every arm of a ladder and every candidate of a run goes through, reachable on its
#' own for a prediction matrix that came from somewhere else: a combination of arms, a model fitted
#' outside the package, predictions read back from a file.
#'
#' A cell is one response in one fold. Scoring only the cells the mask allows is what keeps two
#' arms comparable, so the mask is computed from the response and the fold map alone and never from
#' a model.
#'
#' @param y The response, as a matrix of units by variables.
#' @param p Held-out predictions for the same units and variables.
#' @param folds A [fold_map()] result, or one fold per unit.
#' @param cells A [scorable_cells()] mask. Computed from `y` and `folds` when left unset.
#' @param metric Name of a registered metric to read the cells by, or a function of `(y, p)`.
#'
#' @return A data frame of one row per variable and fold, carrying the score and whether the cell
#'   was scorable.
#'
#' @examples
#' set.seed(1)
#' y <- matrix(rbinom(120, 1, 0.4), nrow = 30,
#'             dimnames = list(sprintf("p%02d", 1:30), paste0("sp", 1:4)))
#' p <- matrix(runif(120), nrow = 30, dimnames = dimnames(y))
#' head(score_predictions(y, p, fold_map(y, v = 3)))
#'
#' @export
score_predictions <- function(y, p, folds, cells = NULL, metric = "roc_auc") {
  y <- .as_response(y)
  f <- .as_folds(folds, rownames(y))
  p <- as.matrix(p)[rownames(y), colnames(y), drop = FALSE]
  if (is.null(cells)) {
    cells <- scorable_cells(y, stats::setNames(f, rownames(y)))
  }
  .score_cells(y, p, f, sort(unique(f)), cells, .as_metric(metric)$fn)
}

# A ladder's levels are calendar grains, and `grain` is the column paired_contrast(),
# grain_contrasts() and occlusion() read by name. One relabelling, here, for the two entry
# points that return a ladder.
.as_grain_rows <- function(rows) {
  names(rows)[names(rows) == "representation"] <- "grain"
  rows
}

# The one rule every level of the package averages cells by. A variable is the independent
# replicate, so a cell mean is taken within a variable over its folds before anything is averaged
# across variables; averaging cells directly would weight a variable by how many folds it happened
# to be scorable in. `by` names the columns that tell arms apart, a ladder's grain and learner or
# a run's candidate, and is empty for the rows of a single arm.
.cell_means <- function(rows, by = character()) {
  keep <- rows[!is.na(rows$score), c(by, "variable", "score"), drop = FALSE]
  if (!nrow(keep)) {
    return(keep)
  }
  stats::aggregate(list(score = keep$score), keep[c(by, "variable")], mean)
}

# The level of each arm, as the mean over the variables `.cell_means()` left it, and how many
# variables that mean is over.
.level_means <- function(per_variable, by) {
  if (!nrow(per_variable)) {
    return(data.frame(per_variable[by], score = numeric(), n_variable = integer()))
  }
  merge(stats::aggregate(list(score = per_variable$score), per_variable[by], mean),
        stats::aggregate(list(n_variable = per_variable$score), per_variable[by], length),
        by = by)
}

# Every attribute the array carries is carried through, rather than a list of the ones a calendar
# grain happens to have: a lookback array carries its span and its lag too, and a fold handed an
# array that has forgotten them is an array nothing downstream can place.
.subset_units <- function(x, idx) {
  out <- x[idx, , , drop = FALSE]
  for (a in setdiff(names(attributes(x)), c("dim", "dimnames", "class"))) {
    attr(out, a) <- attr(x, a)
  }
  if (!is.null(attr(x, "bin_n"))) {
    attr(out, "bin_n") <- attr(x, "bin_n")[idx, , drop = FALSE]
  }
  class(out) <- c("timesift_matrix", "array")
  out
}

.learner_list <- function(learners) {
  if (inherits(learners, "timesift_learner")) {
    learners <- list(learners)
  }
  # A character vector is one name per learner, as the docs promise; `list()` on it would make
  # the whole vector one entry.
  if (is.character(learners)) {
    learners <- as.list(learners)
  }
  learners <- lapply(learners, .as_learner)
  names(learners) <- .fill_names(learners, "name")
  if (anyDuplicated(names(learners))) {
    stop("two learners are reported under the same name: ",
         paste(unique(names(learners)[duplicated(names(learners))]), collapse = ", "),
         ". Name the list to tell them apart.", call. = FALSE)
  }
  learners
}

# A set handed back to `c()` splices rather than nests, so a set can be added to instead of
# rewritten. A learner and a representation are both lists, so what splices is decided by class:
# `is.list()` here would take a single spec apart into its fields.
.splice <- function(args, cls) {
  out <- list()
  for (i in seq_along(args)) {
    a <- args[[i]]
    one <- inherits(a, cls) || !is.list(a)
    out <- c(out, if (one) stats::setNames(list(a), names(args)[i]) else as.list(a))
  }
  out
}

# A learner names itself where the caller did not, and so does a representation; the field each
# names itself by is the only difference between the two cases. Those names are the columns an arm
# is reported under, so a clash between two of them is an error rather than quietly made unique.
.fill_names <- function(x, field) {
  given <- names(x)
  auto <- vapply(x, function(e) e[[field]], character(1L))
  if (is.null(given)) auto else ifelse(nzchar(given), given, auto)
}

#' @export
print.timesift_ladder <- function(x, ...) {
  cat("<timesift ladder>", .plural(length(unique(x$grain)), "grain"), "x",
      .plural(length(unique(x$learner)), "learner"), "\n")
  cat("metric:", attr(x, "metric"), "on", .plural(sum(x$scorable), "scorable cell"), "\n")
  print(summary(x))
  invisible(x)
}

#' @param object A ladder.
#' @rdname grain_ladder
#' @export
summary.timesift_ladder <- function(object, ...) {
  per_variable <- .per_variable(object)
  if (!nrow(per_variable)) {
    return(data.frame(learner = character(), grain = character(), score = numeric(),
                      n_variable = integer(), best = logical(), stringsAsFactors = FALSE))
  }
  out <- .level_means(per_variable, c("learner", "grain"))
  grains <- unique(object$grain)
  out <- out[order(out$learner, match(out$grain, grains), method = "radix"), ]
  # One grain per learner is the best, even where two tie: it is the reference a contrast is read
  # against, and a reference has to be a single grain.
  out$best <- stats::ave(out$score, out$learner,
                         FUN = function(v) seq_along(v) == which.max(v)) == 1
  rownames(out) <- NULL
  out
}

.per_variable <- function(ladder) {
  .cell_means(ladder, c("grain", "learner"))
}

# A level and the spread of the variables it was averaged over, in one place, so a ladder and a
# selection report the same quantity computed the same way.
.mean_se <- function(v) {
  ok <- !is.na(v)
  c(mean(v[ok]), stats::sd(v[ok]) / sqrt(sum(ok)))
}

`%||%` <- function(a, b) if (is.null(a)) b else a
