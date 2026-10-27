#' Choose the grain inside the training data, and score the whole procedure
#'
#' [grain_ladder()] fits every candidate against one fold map and reports the grid, so reading the
#' best grain off it and quoting that grain's score quotes a number the held-out units helped
#' choose. This does the choosing inside the training data instead. Within each outer fold the
#' training units are split again, every candidate is fitted on part of them and scored on the rest,
#' the best is refitted on the whole outer training set, and the outer test fold is predicted once.
#' The estimate that comes back is therefore of the procedure including its choice of grain, which
#' is what an ecologist applying it to a new site would run.
#'
#' A candidate is a `(grain, learner)` pair: the grains are the elements of the representation set,
#' which is where a grain and the statistic its grains are summarised by are both named, and the
#' learners are the ones passed. Both are registry entries or objects built by [learner()], so a new
#' grain, a new grain summary or a new candidate model widens the search with no change here.
#'
#' What the estimate is of: the expected held-out score of the whole pipeline, selection included,
#' on units drawn as these were. What it is not: the score of the winning grain. That is higher, by
#' the amount selection buys itself, and the difference between the two is the quantity this
#' function exists to keep out of a reported number. It also does not say the selected grain is the
#' one a mechanism acts at; it says that grain predicted best on the units the selector saw.
#'
#' The cost is the ladder's, multiplied by the number of inner folds: `v_outer * (v_inner *
#' candidates + 1)` fits. With a neural learner that is where an overnight run goes.
#'
#' @section Choosing a candidate:
#' Inside each outer fold every candidate carries an inner score, the mean over variables of its
#' per-variable mean over the inner folds, and a standard error, the standard deviation over the
#' inner folds of the fold's own score (the mean over the variables scored in that fold) divided by
#' the square root of the number of inner folds. `rule = "argmax"` takes the highest inner score,
#' and on an exact tie the candidate declared first.
#'
#' `rule = "coarsest_adequate"` first finds that highest score and its standard error, calls every
#' candidate scoring at least the highest minus one standard error adequate, and takes the coarsest
#' adequate one. Coarseness is read off the representation as the package holds it: fewer bins is
#' coarser, and between two candidates with the same number of bins, fewer channels is coarser.
#' A tie on both goes to the higher inner score, then to the candidate declared first. Where one
#' candidate scores more than a standard error above every other, the two rules agree; where the
#' inner profile is flat, this one returns the least storage the record can be kept at without a
#' measured loss inside the training data. A standard error that cannot be computed, because a
#' candidate was scored in fewer than two inner folds, is taken as zero, so the rule falls back to
#' the candidates tied with the highest score.
#'
#' @section What the interval is for:
#' The across-variable interval, the one every level of the package reports, is the estimate plus
#' or minus a Student's t quantile times the standard error across the response variables. Its
#' spread is the spread of true skill between variables, and it cannot see the error every
#' variable shares, since all of them are fitted and scored on the same units and the same folds.
#' It is an interval over the variables of this dataset, and not an interval for what the
#' procedure would score on a new sample.
#'
#' `interval = "nested_cv"` adds one that is meant to be, by the nested cross-validation of Bates,
#' Hastie and Tibshirani (2024). Inside every repetition, each outer training set is
#' cross-validated again over the remaining folds of the same map, which gives the mean squared
#' error of a cross-validation estimate as the difference of two terms it can estimate: the
#' squared gap between the inner estimate and the held-out fold's score, less the variance of that
#' fold's score. The paper's error is a mean of per-unit losses; here a fold's score is the mean
#' over the variables scorable in it, the inner estimate is averaged as the reported estimate is,
#' and the variance of a fold's score is its delete-one jackknife variance over the units of the
#' fold, which for a mean of per-unit losses is exactly the paper's `var(e) / |I_k|`. The centre
#' is the nested estimate less the paper's bias correction, its Appendix C, so the interval is
#' for the risk of the procedure fitted on a sample of this size, which is the fit `final` holds.
#' The square root of the estimated mean squared error is held between the jackknife standard
#' error of the estimate and the square root of the fold count times it, as the paper's section
#' 4.3.2 has it.
#'
#' The width departs from the paper in one respect. The paper's width is the mean squared error
#' of the plain cross-validation estimate, and its bias correction is added to the centre as a
#' shift with no spread of its own. The correction is `1 + (K - 2) / K` times the gap between two
#' cross-validation estimates on the same units, and in the package's benchmark
#' (`inst/benchmark/`) that gap moved from sample to sample by more than the estimate it corrects
#' where the sample was small or the signal absent, so the paper's interval covered a nominal 95%
#' at 0.86 to 0.93 there at ten repetitions. The width here is therefore the same identity
#' applied to the corrected estimator: inside every outer training set the nested cross-validation
#' is run once more, over the unordered triples of folds, which gives the bias-corrected estimate
#' that training set alone would report, and the squared gap between that and the held-out fold's
#' score replaces the plain estimate's; the bounds are the corrected centre's own jackknife
#' standard error, with every prediction held fixed, and the square root of the fold count times
#' it. The paper's width is reported beside it as `se_bates` in `nested_cv`. On the package's
#' cheap recovery design (`tests/testthat/test-interval.R`: 150 units, five outer folds, one
#' repetition, 200 replicates) the interval covers a nominal 95% at 0.96 with a signal and 0.94
#' without, where the paper's covers 0.955 and 0.895.
#'
#' The cost is the selection's, multiplied: one repetition fits the procedure once for every
#' unordered pair and every unordered triple of outer folds, `choose(v_outer, 2) +
#' choose(v_outer, 3)` fits, and every repetition after the first refits the outer folds as well.
#' More repetitions steady the estimate of the mean squared error; the paper uses two hundred
#' random splits, which is affordable where a fit is cheap and is not where a fit is a neural
#' network.
#'
#' @references Bates, S., Hastie, T. and Tibshirani, R. (2024). Cross-validation: what does it
#'   estimate and how well does it do it? *Journal of the American Statistical Association*
#'   **119**(546), 1434-1445. \doi{10.1080/01621459.2023.2197686}
#'
#' @section A cut learned inside the training data:
#' TSS read at the cut that maximises it on the scored units is biased upward, most where presences
#' are few ([tss_inflation()]). With `threshold` set, each outer fold learns one cut per variable
#' on the inner out-of-fold predictions of the candidate it selected, which the inner search has
#' already made for every outer training unit, by [decision_threshold()] under the rule named. The
#' cut is then frozen and the outer test fold's predictions are read at it by [tss()]. No unit of
#' an outer test fold enters the cut its own fold is read at. The inner out-of-fold predictions
#' come from models fitted on part of the outer training set and the held-out predictions from
#' the refit on all of it, so the cut is learned on predictions of the same candidate from slightly
#' smaller training sets.
#'
#' @param x A [grain_matrix()] result, a [timesift_set()], or a named list of representations.
#'   Its names are the grains being chosen between.
#' @param y The response for the same units.
#' @param learners A learner, a list of them, or names of registered ones, as [grain_ladder()]
#'   takes. Named alongside the grains they form the candidate set.
#' @param folds The outer fold map, from [fold_map()] or any named integer vector. Built with the
#'   defaults of [fold_map()] when not given.
#' @param inner Number of inner folds the selection is made on, or a function of the outer training
#'   response returning a fold map for those units. A count deals the inner folds by the grouping
#'   the outer fold map carries, so what [grouped_cv()] kept whole outside stays whole inside.
#' @param rule How a candidate is chosen from its inner scores. `"argmax"` takes the highest.
#'   `"coarsest_adequate"` takes the coarsest candidate whose inner score lies within one standard
#'   error of the highest, the one-standard-error rule of Breiman, Friedman, Olshen and Stone
#'   (1984) and of Hastie, Tibshirani and Friedman (2009, section 7.10) with coarseness in place of
#'   model complexity. See Choosing a candidate.
#' @param threshold `NULL`, or the rule of [decision_threshold()] a presence-absence cut is learned
#'   by: `"youden"`, the cut that maximises TSS, `"kappa"` or `"prevalence"`. See A cut learned
#'   inside the training data.
#' @param interval Which interval to report beside the across-variable one, which is always
#'   reported: `"variables"` for that one alone, or `"nested_cv"` for an interval for the
#'   procedure's risk. See What the interval is for.
#' @param repeats Repetitions of the nested cross-validation, each on its own fold map. The first
#'   is the map the estimate was computed on.
#' @param response Name of the registered response head.
#' @param metric Name of a registered metric the selection is made on, or `NULL` for the
#'   response's own. The estimate is reported under every registered metric whichever this is.
#' @param compare A [grain_ladder()] result on the same units, response and outer fold map, whose
#'   arms the selected procedure is contrasted against cell by cell. `NULL` for no contrast.
#' @param control [train_control()], the training settings every neural learner reads, in the
#'   inner search and in the refit alike. A learner carrying a control of its own overrides it on
#'   the settings that control names.
#' @param seed Seed for the inner splits. Each outer fold splits under `seed` plus its own number,
#'   so no two outer folds inherit the same inner partition.
#' @param verbose Report each outer fold and what it selected as it runs.
#'
#' @return A `timesift_selection`: a list carrying `selected`, one row per outer fold with the
#'   candidate it chose, the inner score it chose on, the highest inner score in that fold
#'   (`inner_best`) and that score's standard error (`inner_se`); `estimate`, the nested score under every
#'   registered metric with its standard error across variables; `contrast`, one
#'   [paired_contrast()] row against each arm of `compare`, or `NULL`; `candidates`, the set that
#'   was searched; and `scores`, the per-cell rows of the selected procedure under the selection
#'   metric, in the layout [grain_ladder()] returns. The held-out prediction of every unit is in
#'   the `predictions` attribute and the scorable-cell mask in `cells`. `inner` holds every
#'   candidate's inner score and standard error in every outer fold. With `threshold` set, the
#'   estimate carries the score, its interval and the interval's name in `interval`, one row per
#'   metric and interval. With `interval = "nested_cv"` it also carries `nested_cv`, the same rows
#'   with the estimator's own quantities beside them, and `final`, the procedure fitted on every
#'   unit, whose risk that interval is for. With `threshold` set, the
#'   estimate carries one further row, `tss_inner_cut`, the procedure's TSS at the learned cuts;
#'   `thresholds` holds the cut of every outer fold and variable; and `cut_scores` the per-cell
#'   rows it is averaged from, in the layout of `scores`. Both are `NULL` otherwise.
#'
#' @seealso [grain_ladder()] for the grid this selects from, and [paired_contrast()] for the
#'   comparison the `contrast` element holds.
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
#' sel <- select_grain(x, y, elasticnet(), folds = fold_map(y, v = 3), inner = 3,
#'                     verbose = FALSE)
#' sel
#' sel$estimate
#'
#' @export
select_grain <- function(x, y, learners, folds = NULL, inner = 5L,
                         rule = c("argmax", "coarsest_adequate"), threshold = NULL,
                         interval = c("variables", "nested_cv"), repeats = 1L,
                         response = "presence_absence", metric = NULL, compare = NULL,
                         control = train_control(), seed = 1L, verbose = TRUE) {
  rule <- match.arg(rule)
  interval <- .check_interval(interval[1L])
  if (!is.null(threshold)) {
    threshold <- match.arg(threshold, c("youden", "kappa", "prevalence"))
  }
  set <- .as_set(x)
  units <- dimnames(set[[1L]])[[1L]]
  spec <- .responses_reg$get(response)
  y <- .align_response(spec$prepare(y), units)
  if (is.null(folds)) {
    folds <- fold_map(y)
  }
  f <- .as_folds(folds, units)
  cells <- spec$cells(y, stats::setNames(f, units))
  # The estimate is reported under every registered metric, and one selected on has to be a row
  # of that table, so this is the one door a function of (y, p) does not go through.
  if (is.function(metric)) {
    stop("`select_grain()` reports the estimate under every registered metric, so the one it ",
         "selects on has to be registered. register_metric() takes a function of (y, p).",
         call. = FALSE)
  }
  metric <- metric %||% spec$metric
  score <- .metrics_reg$get(metric)
  learners <- .learner_list(learners)
  group <- .fold_group(folds, units)
  inner_split <- .inner_splitter(inner, group)
  .check_compare(compare, metric, interval)
  candidates <- expand.grid(grain = names(set), learner = names(learners),
                            KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
  if (nrow(candidates) < 2L) {
    stop("selection needs at least two candidates; got one grain and one learner.", call. = FALSE)
  }
  ctx <- .selection_context(set, y, learners, candidates, .cross_pairs(set, learners), rule,
                            inner_split, response, metric, control, group)

  levels <- sort(unique(f))
  p <- matrix(NA_real_, nrow = length(units), ncol = ncol(y),
              dimnames = list(units, colnames(y)))
  chosen <- vector("list", length(levels))
  inner_scores <- vector("list", length(levels))
  cuts <- matrix(NA_real_, nrow = length(levels), ncol = ncol(y),
                 dimnames = list(as.character(levels), colnames(y)))

  for (i in seq_along(levels)) {
    k <- levels[i]
    train <- which(f != k)
    test <- which(f == k)
    y_train <- y[train, , drop = FALSE]
    once <- .select_once(ctx, train, test, seed + i, fold = k)
    lad <- once$lad
    grid <- once$grid
    best <- once$best
    won <- once$won
    p[rownames(once$pred), colnames(once$pred)] <- once$pred

    # The cut is learned on the selected candidate's inner out-of-fold predictions, which cover
    # the outer training units and nothing else.
    if (!is.null(threshold)) {
      oof <- attr(lad, "predictions")[[.candidate_label(grid$grain[won], grid$learner[won])]]
      cuts[i, ] <- vapply(colnames(y), function(v) {
        decision_threshold(y_train[, v], oof[rownames(y_train), v], threshold)
      }, numeric(1L))
    }

    chosen[[i]] <- data.frame(fold = k, grain = grid$grain[won], learner = grid$learner[won],
                              inner_score = grid$score[won], inner_best = grid$score[best],
                              inner_se = grid$se[best], n_train = length(train),
                              n_test = length(test), stringsAsFactors = FALSE)
    inner_scores[[i]] <- grid[c("fold", "grain", "learner", "score", "se", "n_variable")]
    if (verbose) {
      message(sprintf("fold %s of %d selected %s|%s at %s %.3f", k, length(levels),
                      grid$grain[won], grid$learner[won], metric, grid$score[won]))
    }
  }

  selected <- do.call(rbind, chosen)
  scores <- .as_grain_rows(.score_arm(.selected_label, "selected", y, p, f, levels, cells, score))
  scores <- structure(scores, class = c("timesift_ladder", "data.frame"),
                      predictions = stats::setNames(list(p), .selected_arm),
                      cells = cells, folds = stats::setNames(f, units),
                      metric = metric, scorer = score, response = response)

  estimate <- .nested_estimate(y, p, f, levels, cells)
  ncv <- nested <- final <- NULL
  if (interval == "nested_cv") {
    # The procedure fitted once on every unit is the model the interval is for.
    whole <- .select_once(ctx, seq_along(units), integer(), seed, fold = NA_integer_)
    final <- list(grain = whole$grid$grain[whole$won], learner = whole$grid$learner[whole$won],
                  fit = whole$fit, inner = whole$grid)
    maps <- .ncv_maps(y, f, group, repeats, seed)
    if (verbose) {
      message("nested cross-validation: ", length(maps), " repetition(s) of ",
              length(levels), " outer folds")
    }
    runs <- .ncv_collect(function(train, test, tag) {
      .select_once(ctx, train, test, .ncv_seed(seed, tag))$pred
    }, y, maps, p)
    ncv <- .ncv_record(y, maps, stats::setNames(list(.ncv_keep(runs)), .selected_arm))
    attr(scores, "ncv") <- ncv
    nested <- .nested_cv_estimate(ncv, .selected_arm, response)
    estimate <- rbind(estimate, nested[names(estimate)])
  }
  thresholds <- cut_scores <- NULL
  if (!is.null(threshold)) {
    thresholds <- data.frame(fold = rep(levels, times = ncol(y)),
                             variable = rep(colnames(y), each = length(levels)),
                             threshold = as.vector(cuts), rule = threshold,
                             stringsAsFactors = FALSE)
    cut_scores <- .as_grain_rows(.score_arm(.selected_label, "selected", y, p, f, levels, cells,
                                            tss, at = cuts))
    cut_scores <- structure(cut_scores, class = c("timesift_ladder", "data.frame"),
                            cells = cells, folds = stats::setNames(f, units),
                            metric = .inner_cut_metric, response = response)
    estimate <- rbind(estimate, .estimate_row(.inner_cut_metric, cut_scores))
  }

  out <- list(
    selected = selected,
    estimate = estimate,
    contrast = .selection_contrast(scores, compare, interval),
    candidates = candidates,
    scores = scores,
    inner = do.call(rbind, inner_scores),
    thresholds = thresholds,
    cut_scores = cut_scores,
    nested_cv = nested,
    final = final
  )
  structure(out, class = "timesift_selection", metric = metric, response = response,
            rule = rule, threshold = threshold, interval = interval,
            folds = stats::setNames(f, units), cells = cells,
            predictions = stats::setNames(list(p), .selected_arm))
}

# The selected procedure is one arm like any other, so it carries an arm label of the ladder's own
# shape and every reader that splits on "|" keeps working.
.selected_label <- "selected"
.selected_arm <- "selected|selected"

#' @export
print.timesift_selection <- function(x, ...) {
  cat("<timesift selection>", .plural(nrow(x$selected), "outer fold"), "over",
      .plural(nrow(x$candidates), "candidate"), "by", attr(x, "rule") %||% "argmax", "\n")
  est <- x$estimate[x$estimate$metric == attr(x, "metric"), , drop = FALSE]
  cat(sprintf("%s: %.3f for the procedure, selection included\n", attr(x, "metric"),
              est$score[1L]))
  for (r in seq_len(nrow(est))) {
    cat(sprintf("  95%% interval %.3f to %.3f (se %.3f), for %s\n", est$lower[r], est$upper[r],
                est$se[r], .interval_target(est$interval[r])))
  }
  cut <- x$estimate[x$estimate$metric == .inner_cut_metric, , drop = FALSE]
  if (nrow(cut)) {
    cat(sprintf("tss at the %s cut learned on the inner folds: %.3f (se %.3f)\n",
                attr(x, "threshold"), cut$score, cut$se))
  }
  print(summary(x))
  invisible(x)
}

#' @param object A selection.
#' @param ... Ignored.
#' @rdname select_grain
#' @export
summary.timesift_selection <- function(object, ...) {
  out <- object$candidates
  key <- .candidate_label(out$grain, out$learner)
  picked <- .candidate_label(object$selected$grain, object$selected$learner)
  out$n_selected <- as.integer(table(factor(picked, levels = key)))
  out$share <- out$n_selected / nrow(object$selected)
  inner <- object$inner
  mean_inner <- tapply(inner$score, .candidate_label(inner$grain, inner$learner), mean,
                       na.rm = TRUE)
  out$inner_score <- as.numeric(mean_inner[key])
  out <- out[order(-out$n_selected, -out$inner_score), ]
  rownames(out) <- NULL
  out
}

#' Draw how stable the choice of grain was
#'
#' Every candidate's inner score in every outer fold, one line per outer fold, with an open circle
#' on the candidate that fold selected. A selection that lands on the same candidate each time draws
#' its circles in one column; one that wanders says the grid is flat enough that the choice is
#' arbitrary, which is worth seeing beside the estimate rather than after it.
#'
#' @param x A [select_grain()] result.
#' @param col One colour per outer fold, recycled.
#' @param ... Passed to [graphics::plot()].
#'
#' @return The table of inner scores the plot is drawn from, invisibly, with `at`, the position of
#'   each candidate on the axis, and `selected`, whether its fold chose it.
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
#' sel <- select_grain(x, y, elasticnet(), folds = fold_map(y, v = 3), inner = 3,
#'                     verbose = FALSE)
#' plot(sel)
#'
#' @export
plot.timesift_selection <- function(x, col = NULL, ...) {
  inner <- x$inner
  label <- .candidate_label(x$candidates$grain, x$candidates$learner)
  at <- seq_along(label)
  inner$at <- match(.candidate_label(inner$grain, inner$learner), label)
  inner$selected <- paste(inner$fold, inner$at) %in%
    paste(x$selected$fold, match(.candidate_label(x$selected$grain, x$selected$learner), label))
  folds <- sort(unique(inner$fold))
  if (is.null(col)) {
    col <- grDevices::hcl.colors(max(length(folds), 2L), "Dark 3")[seq_along(folds)]
  }
  col <- rep_len(col, length(folds))

  # The candidate names are written up the axis, so the bottom margin is sized to the longest.
  old <- graphics::par(mar = c(max(5.1, 1.5 + 0.8 * max(nchar(label)) * 0.55), 4.1, 2.1, 1.1))
  on.exit(graphics::par(old), add = TRUE)
  span <- range(inner$score[is.finite(inner$score)])
  args <- list(x = at, y = rep(NA_real_, length(at)), ylim = span, xaxt = "n", xlab = "",
               ylab = paste(attr(x, "metric"), "inside the training data"))
  do.call(graphics::plot, utils::modifyList(args, list(...)))
  graphics::axis(1L, at = at, labels = label, las = 2L, cex.axis = 0.8)
  graphics::grid(nx = NA, ny = NULL, col = "grey90", lty = 1L)

  for (i in seq_along(folds)) {
    rows <- inner[inner$fold == folds[i], , drop = FALSE]
    rows <- rows[order(rows$at), , drop = FALSE]
    graphics::lines(rows$at, rows$score, col = col[i], lwd = 1.5)
    graphics::points(rows$at, rows$score, col = col[i], pch = 19L, cex = 0.8)
    won <- rows[rows$selected, , drop = FALSE]
    graphics::points(won$at, won$score, col = col[i], pch = 1L, cex = 2.2, lwd = 2)
  }
  invisible(inner)
}

# A candidate is named as the ladder names an arm, grain and learner joined by a bar, wherever a
# selection's tables are matched against each other.
.candidate_label <- function(grain, learner) paste(grain, learner, sep = "|")

# Every registered metric reads the same held-out predictions, so the estimate is reported under all
# of them and the choice of selection metric does not decide what may be quoted.
.nested_estimate <- function(y, p, f, levels, cells) {
  out <- lapply(metrics(), function(nm) {
    .estimate_row(nm, .score_arm(.selected_label, "selected", y, p, f, levels, cells,
                                 .metrics_reg$get(nm)))
  })
  out <- do.call(rbind, out)
  rownames(out) <- NULL
  out
}

# One row of the estimate: the mean over variables of each variable's mean over its scored cells,
# and the interval across variables, on Student's t with one degree of freedom fewer than there
# are variables.
.estimate_row <- function(name, rows) {
  per_variable <- .cell_means(rows)
  ms <- .mean_se(per_variable$score)
  half <- .t_margin(ms[2L], nrow(per_variable))
  data.frame(metric = name, score = ms[1L], center = ms[1L], se = ms[2L], lower = ms[1L] - half,
             upper = ms[1L] + half, n_variable = nrow(per_variable), interval = "variables",
             stringsAsFactors = FALSE)
}

# The nested cross-validation rows of the estimate, one per registered metric, each read off the
# same stored predictions. The diagnostics of the paper's estimator ride along for inspection.
.nested_cv_estimate <- function(ncv, arm, response) {
  out <- lapply(metrics(), function(nm) {
    iv <- .ncv_read(ncv, arm, response, .metrics_reg$get(nm))
    cbind(data.frame(metric = nm, score = iv$estimate, center = iv$center, se = iv$se,
                     lower = iv$lower, upper = iv$upper, n_variable = iv$n_variable,
                     interval = "nested_cv", stringsAsFactors = FALSE),
          iv[c("bias", "err_ncv", "mse_ncv", "mse_center", "se_bates", "se_naive",
               "se_naive_center", "repeats", "folds")])
  })
  out <- do.call(rbind, out)
  rownames(out) <- NULL
  out
}

# The label the TSS read at a cut learned on the inner folds is reported under. It is not a
# registered metric: a cut learned elsewhere is not a function of one cell's (y, p).
.inner_cut_metric <- "tss_inner_cut"

# The contrast is the ladder's, run on one table holding both arms, so the pairing rule and the
# interval come from paired_contrast() rather than from a second copy of it here.
.check_compare <- function(compare, metric, interval = "variables") {
  if (is.null(compare)) {
    return(invisible(TRUE))
  }
  if (!inherits(compare, "timesift_ladder")) {
    stop("`compare` is a grain_ladder() result, got ", class(compare)[1L], ".", call. = FALSE)
  }
  if (!identical(attr(compare, "metric"), metric)) {
    stop("`compare` is scored by ", attr(compare, "metric"), " and the selection by ", metric,
         ". Score both by the same metric before contrasting them.", call. = FALSE)
  }
  if (identical(interval, "nested_cv") && is.null(.ncv_of(compare))) {
    stop("`compare` was fitted without nested cross-validation, so its contrast with the ",
         "selection has none to read. Fit it with grain_ladder(interval = \"nested_cv\") on the ",
         "same folds, `repeats` and `seed`.", call. = FALSE)
  }
  invisible(TRUE)
}

# The across-variable interval of every contrast, and beside it the nested cross-validation one
# where the selection was fitted with it.
.selection_contrast <- function(scores, compare, interval = "variables") {
  if (is.null(compare)) {
    return(NULL)
  }
  shared <- intersect(names(scores), names(compare))
  both <- rbind(scores[shared], compare[shared])
  both <- structure(both, class = c("timesift_ladder", "data.frame"),
                    metric = attr(scores, "metric"), scorer = attr(scores, "scorer"),
                    response = attr(scores, "response"),
                    ncv = .ncv_join(.ncv_of(scores), .ncv_of(compare)))
  arms <- unique(.candidate_label(compare$grain, compare$learner))
  kinds <- unique(c("variables", interval))
  out <- lapply(kinds, function(kind) {
    do.call(rbind, lapply(arms, function(a) paired_contrast(both, .selected_arm, a, kind)))
  })
  out <- do.call(rbind, out)
  rownames(out) <- NULL
  out
}

# What one fit of a selection procedure reads. `candidates` is the set in the order a tie is broken
# in, its `grain` naming the representation and its `learner` the learner; `pairs` is the same set
# in the order it is fitted in.
.selection_context <- function(set, y, learners, candidates, pairs, rule, inner_split, response,
                               metric, control, group) {
  list(set = set, y = y, learners = learners, candidates = candidates, pairs = pairs,
       size = .candidate_size(set, candidates), rule = rule, inner_split = inner_split,
       response = response, metric = metric, control = control, group = group)
}

# One fit of the whole procedure: the inner search on the training units, the rule, the refit of
# the chosen candidate on all of them, and its predictions for the test units. The outer folds of
# select_grain() and every fit of its nested cross-validation go through this.
.select_once <- function(ctx, train, test, split_seed, fold = NA_integer_) {
  search <- .inner_search(ctx, train, split_seed, fold)
  refit <- .refit_candidates(ctx, search$won, train, test)
  c(search, list(fit = refit$fits[[1L]], pred = refit$preds[[1L]]))
}

# The search half of a selection: every candidate cross-validated on an inner map drawn on the
# training units, and the rule applied to those inner scores. The selector sees the training units
# and nothing else: the inner map is drawn on them, and the representation it searches over is cut
# to them before any fitting happens.
.inner_search <- function(ctx, train, split_seed, fold = NA_integer_) {
  y_train <- ctx$y[train, , drop = FALSE]
  lad <- .ladder(.subset_set(ctx$set, train), y_train, ctx$learners, ctx$pairs,
                 folds = ctx$inner_split(y_train, split_seed, train),
                 response = ctx$response, metric = ctx$metric, control = ctx$control,
                 verbose = FALSE)
  grid <- .join_candidates(ctx$candidates, summary(lad), .inner_se(lad), fold)
  if (all(!is.finite(grid$score))) {
    stop("no candidate scored inside the training data of ",
         if (is.na(fold)) "a fit of the nested cross-validation" else paste("fold", fold),
         ". Widen the inner folds or drop the variables that cannot be scored.", call. = FALSE)
  }
  list(lad = lad, grid = grid, best = .first_best(grid$score),
       won = .choose_candidate(grid, ctx$size, ctx$rule))
}

# The refit half: the named candidates, rows of `ctx$candidates`, each fitted on every training unit
# and asked for the test units. This is the fitting path of the inner ladder itself, so a selected
# candidate's held-out prediction is the one it would have made there.
.refit_candidates <- function(ctx, rows, train, test) {
  y_train <- ctx$y[train, , drop = FALSE]
  out <- lapply(rows, function(r) {
    x <- ctx$set[[ctx$candidates$grain[r]]]
    fit <- fit_learner(ctx$learners[[ctx$candidates$learner[r]]], .subset_units(x, train),
                       y_train, response = ctx$response, control = ctx$control,
                       group = ctx$group[train])
    pred <- if (length(test)) stats::predict(fit, .subset_units(x, test)) else NULL
    list(fit = fit, pred = pred)
  })
  list(fits = lapply(out, `[[`, "fit"), preds = lapply(out, `[[`, "pred"))
}

# The candidate set keeps the order its grains and its learners were declared in, so which
# candidate an exact tie on the inner score falls to does not depend on the session's collation the
# way a join on the names would.
.join_candidates <- function(candidates, grid, se, fold) {
  key <- .candidate_label(candidates$grain, candidates$learner)
  i <- match(key, .candidate_label(grid$grain, grid$learner))
  candidates$score <- grid$score[i]
  candidates$se <- unname(se[key])
  candidates$n_variable <- grid$n_variable[i]
  candidates$fold <- fold
  candidates
}

# The standard error of each candidate's inner score: the spread over the inner folds of the fold's
# own score, the mean over the variables scored in it, divided by the square root of their number.
# Named by the candidate's "grain|learner" label.
.inner_se <- function(lad) {
  keep <- lad[!is.na(lad$score), , drop = FALSE]
  if (!nrow(keep)) {
    return(stats::setNames(numeric(), character()))
  }
  by_fold <- stats::aggregate(list(score = keep$score), keep[c("grain", "learner", "fold")], mean)
  key <- .candidate_label(by_fold$grain, by_fold$learner)
  vapply(split(by_fold$score, key), function(s) {
    if (length(s) < 2L) NA_real_ else stats::sd(s) / sqrt(length(s))
  }, numeric(1L))
}

# The highest inner score, and on an exact tie the candidate declared first.
.first_best <- function(score) {
  which.max(ifelse(is.finite(score), score, -Inf))
}

# How coarse each candidate is, read off the representation it reads: its number of bins and its
# number of channels.
.candidate_size <- function(set, candidates) {
  d <- lapply(candidates$grain, function(g) dim(set[[g]]))
  data.frame(bins = vapply(d, `[`, numeric(1L), 2L),
             channels = vapply(d, `[`, numeric(1L), 3L))
}

# The rule a candidate is chosen by. Under "coarsest_adequate" every candidate within one standard
# error of the highest score is adequate, and the coarsest of them wins: fewest bins, then fewest
# channels, then the higher score, then the order of declaration.
.choose_candidate <- function(grid, size, rule) {
  best <- .first_best(grid$score)
  if (identical(rule, "argmax")) {
    return(best)
  }
  score <- ifelse(is.finite(grid$score), grid$score, -Inf)
  se <- grid$se[best]
  tolerance <- if (is.finite(se)) se else 0
  adequate <- which(score >= score[best] - tolerance)
  adequate[order(size$bins[adequate], size$channels[adequate], -score[adequate], adequate)][1L]
}

# The inner map is drawn on the outer training units alone, either by fold_map() at a given count,
# dealing by the grouping the outer map carries, or by a splitter of the caller's own.
.inner_splitter <- function(inner, group = NULL) {
  if (is.function(inner)) {
    return(function(y_train, seed, train) inner(y_train))
  }
  if (!is.numeric(inner) || length(inner) != 1L || is.na(inner) || inner < 2L ||
        inner != trunc(inner)) {
    stop("`inner` is a number of folds of at least 2, or a function of the training response, got ",
         paste(deparse(inner), collapse = ""), ".", call. = FALSE)
  }
  inner <- as.integer(inner)
  function(y_train, seed, train) {
    fold_map(y_train, v = inner, seed = seed, strata = if (is.null(group)) 5L else 1L,
             group = group[train])
  }
}

.subset_set <- function(set, idx) {
  timesift_set(lapply(set, .subset_units, idx = idx))
}
