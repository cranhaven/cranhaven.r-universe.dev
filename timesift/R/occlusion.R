#' What part of the record a fitted model reads
#'
#' Holds one bin of the record back at a time, rescores the held-out units, and records the fall in
#' score as that bin's weight. Nothing is refitted: the models kept by
#' `grain_ladder(keep_fits = TRUE)` are the ones read, so the profile describes the models that
#' produced the reported scores rather than a fresh set of them.
#'
#' A model has to be shown something in place of a held-back bin, and what it is shown decides what
#' the weight means. Permuting the bin's values across units keeps the observed readings exactly
#' and cuts only the link between a reading and its unit. Replacing every unit by the fitting-fold
#' mean removes all between-unit variation while keeping the shape of the year. Replacing the bin
#' by each unit's own mean over the record keeps how warm a unit is and removes only that bin's
#' departure from it.
#'
#' A bin is held back whole: a permutation moves every channel of the bin together, so a unit is
#' shown another unit's week rather than a coldest day from one unit beside a warmest day from
#' another. A channel that is the same for every unit, as [calendar_channels()] are, says where the
#' bin sits rather than what a unit read there, and is left in place.
#'
#' Read with `over = "channel"` the same machinery asks what each statistic of a grain carries,
#' holding one channel back across the whole record instead of one bin across all channels.
#'
#' @param x A [timesift()] fit, or a [grain_ladder()] result, in either case fitted with
#'   `keep_fits = TRUE`.
#' @param data The representation set the ladder was fitted on.
#' @param y The response it was fitted to.
#' @param arm The arm to read, as `"grain|learner"` or `"learner"`.
#' @param ... Passed to the method.
#' @param over `"bin"` to hold each bin back in turn, `"channel"` for each channel.
#' @param substitute What a held-back part is replaced by: `"permute"`, `"fold_mean"` or
#'   `"unit_mean"`.
#' @param metric Name of the registered metric the rescoring is read by, or a function of
#'   `(y, p)`. Left unset it is the one the fit was scored under, so a weight is a fall in the
#'   number [summary()] reports rather than in a second one. `"roc_auc"` is usually the steadier
#'   reading over many rescorings: it responds to every reordering of the units, where a maximum
#'   over thresholds frequently does not move at all.
#' @param permutations Draws averaged over, for `substitute = "permute"`.
#' @param seed Random seed.
#'
#' @return A data frame of one row per held-back part and variable, carrying the mean weight over
#'   folds and the score with and without the part.
#'
#' @examples
#' set.seed(1)
#' t <- seq(as.POSIXct("2021-09-01", tz = "UTC"), by = "hour", length.out = 24 * 120)
#' units <- sprintf("p%02d", 1:40)
#' warmth <- rnorm(40)
#' d <- data.frame(
#'   plot = rep(units, each = length(t)), t = rep(t, length(units)),
#'   temp = as.numeric(vapply(warmth, function(w) w + sin(seq_along(t) / 300) + rnorm(length(t)),
#'                            numeric(length(t)))))
#' y <- matrix(rbinom(80, 1, plogis(c(warmth, -warmth))), nrow = 40,
#'             dimnames = list(units, c("sp1", "sp2")))
#' x <- grain_matrix(d, plot, t, temp, grain = "month")
#' lad <- grain_ladder(x, y, elasticnet(), folds = fold_map(y, v = 3),
#'                      keep_fits = TRUE, verbose = FALSE)
#' head(occlusion(lad, x, y, "month|elasticnet", permutations = 3))
#'
#' @export
occlusion <- function(x, ...) UseMethod("occlusion")

#' @rdname occlusion
#' @export
occlusion.default <- function(x, ...) {
  stop("expected a timesift() run or a grain_ladder() result, got ", class(x)[1L], ".",
       call. = FALSE)
}

#' @rdname occlusion
#' @export
occlusion.timesift_ladder <- function(x, data, y, arm, over = c("bin", "channel"),
                                     substitute = c("permute", "fold_mean", "unit_mean"),
                                     metric = NULL, permutations = 20L, seed = 1L,
                                     ...) {
  ladder <- x
  x <- data
  over <- match.arg(over)
  substitute <- match.arg(substitute)
  fits <- attr(ladder, "fits")
  if (is.null(fits)) {
    stop("this ladder kept no fits. Refit with grain_ladder(..., keep_fits = TRUE).",
         call. = FALSE)
  }
  rows <- .arm_rows(ladder, .best_arm(ladder, arm))
  label <- attr(rows, "label")
  grain <- attr(rows, "grain")
  set <- .as_set(x)
  m <- set[[grain]]
  if (is.null(m)) {
    stop("the representation carries no \"", grain, "\" grain.", call. = FALSE)
  }
  units <- dimnames(m)[[1L]]
  y <- .align_response(.responses_reg$get(attr(ladder, "response"))$prepare(y), units)
  f <- .as_folds(attr(ladder, "folds"), units)
  cells <- attr(ladder, "cells")
  # Left unset the profile is read by the metric the fit was scored under, so a weight is a fall
  # in the number the summary reports rather than in a second one.
  metric <- if (is.null(metric)) list(fn = attr(ladder, "scorer"), name = attr(ladder, "metric"))
            else .as_metric(metric)
  score <- metric$fn

  parts <- if (over == "bin") seq_len(dim(m)[2L]) else seq_len(dim(m)[3L])
  labels <- if (over == "bin") dimnames(m)[[2L]] else dimnames(m)[[3L]]
  held <- .unit_varying(m)

  old <- .seed_state()
  on.exit(.restore_seed(old), add = TRUE)
  set.seed(seed)

  out <- list()
  for (k in sort(unique(f))) {
    fit <- fits[[paste(label, k, sep = "|")]]
    if (is.null(fit)) {
      next
    }
    test <- which(f == k)
    train <- which(f != k)
    base <- stats::predict(fit, .subset_units(m, test))
    ok <- .scorable_for(cells, colnames(y), k)
    full <- .score_columns(y[test, , drop = FALSE], base, ok, score)

    for (i in seq_along(parts)) {
      draws <- if (substitute == "permute") permutations else 1L
      acc <- matrix(0, nrow = draws, ncol = ncol(y))
      for (r in seq_len(draws)) {
        occluded <- .occlude(m, test, train, parts[i], over, substitute, held)
        acc[r, ] <- .score_columns(y[test, , drop = FALSE],
                                   stats::predict(fit, occluded), ok, score)
      }
      out[[length(out) + 1L]] <- data.frame(
        part = labels[i], variable = colnames(y), fold = k,
        score_full = full, score_held_back = colMeans(acc),
        weight = full - colMeans(acc), stringsAsFactors = FALSE)
    }
  }
  out <- do.call(rbind, out)
  out <- out[!is.na(out$weight), , drop = FALSE]
  agg <- stats::aggregate(out[c("score_full", "score_held_back", "weight")],
                          out[c("part", "variable")], mean)
  agg$part <- factor(agg$part, levels = labels)
  agg <- agg[order(agg$part, agg$variable, method = "radix"), ]
  agg$part <- as.character(agg$part)
  rownames(agg) <- NULL
  structure(agg, class = c("timesift_occlusion", "data.frame"), arm = label, over = over,
            substitute = substitute, metric = metric$name)
}

.occlude <- function(m, test, train, i, over, substitute, held) {
  sub <- .subset_units(m, test)
  n <- dim(sub)[1L]
  b <- dim(sub)[2L]
  if (over == "channel") {
    sub[, , i] <- switch(
      substitute,
      permute = .plane(sub, i)[sample.int(n), , drop = FALSE],
      fold_mean = matrix(colMeans(.plane(m, i)[train, , drop = FALSE]),
                         nrow = n, ncol = b, byrow = TRUE),
      unit_mean = matrix(rowMeans(.plane(sub, i)), nrow = n, ncol = b)
    )
    return(sub)
  }
  order <- if (substitute == "permute") sample.int(n) else NULL
  for (ch in held) {
    sub[, i, ch] <- switch(
      substitute,
      permute = sub[order, i, ch],
      fold_mean = mean(m[train, i, ch]),
      unit_mean = rowMeans(.plane(sub, ch))
    )
  }
  sub
}

# The channels that differ between units somewhere in the record. The rest are the same for every
# unit, as the calendar channels are, and holding a bin back leaves them where they are.
.unit_varying <- function(m) {
  which(vapply(seq_len(dim(m)[3L]), function(ch) {
    p <- .plane(m, ch)
    any(p != rep(p[1L, ], each = nrow(p)))
  }, logical(1L)))
}

# One channel of a representation as a [unit, bin] matrix, whatever the number of bins. Dropping to
# a vector at a single bin is what makes the row and column means below silently wrong.
.plane <- function(x, channel) {
  matrix(as.numeric(x[, , channel]), nrow = dim(x)[1L], ncol = dim(x)[2L])
}

.scorable_for <- function(cells, variables, fold) {
  cells$scorable[match(paste(variables, fold), paste(cells$variable, cells$fold))]
}

.score_columns <- function(y, p, ok, score) {
  vapply(seq_len(ncol(y)), function(j) {
    if (isTRUE(ok[j])) score(y[, j], p[, j]) else NA_real_
  }, numeric(1L))
}

#' @export
print.timesift_occlusion <- function(x, ...) {
  cat("<timesift occlusion>", attr(x, "arm"), "read by", attr(x, "metric"), "\n")
  cat("held back:", attr(x, "over"), "; substitute:", attr(x, "substitute"), "\n")
  w <- stats::aggregate(list(weight = x$weight), x["part"], mean)
  w <- w[order(-w$weight), ]
  cat("heaviest:", paste(utils::head(w$part, 5L), collapse = ", "), "\n")
  print(utils::head(as.data.frame(x), 6L))
  invisible(x)
}
