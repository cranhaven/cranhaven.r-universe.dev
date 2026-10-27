#' Assign units to cross-validation folds
#'
#' One fold map, built once and read by everything that scores. Every learner in a ladder is then
#' fitted and scored on identical splits, which is what makes the comparison between them paired
#' rather than a comparison of two clouds of numbers.
#'
#' Units are held out singly. Where the input a model reads is measured at the unit itself, as a
#' logger in each plot is, a held-out unit brings its own measured input with it and nothing of its
#' neighbours' reaches the model.
#'
#' Folds are balanced within strata: units are grouped into `strata` equal-count groups of the
#' stratifying value, shuffled inside each group, and dealt round-robin by one counter that runs
#' on from each stratum into the next, so each fold carries the same mix and the folds are equal
#' in size to within one unit whatever `v` is, up to one unit per fold. With a multi-variable
#' response the default stratifies on richness, the number of variables present at a unit,
#' because one fold map has to serve every variable at once and cannot be stratified on any
#' single one of them.
#'
#' @param y The response: a matrix or data frame of units by variables, with unit identifiers in
#'   the row names or in a leading character or factor column.
#' @param v Number of folds.
#' @param seed Random seed, fixed so the map is reproducible.
#' @param strata Number of strata, or `1` for no stratification.
#' @param by A numeric vector of length `nrow(y)` to stratify on instead of richness.
#' @param group A vector of length `nrow(y)`. Rows sharing a value land in one fold, which is what
#'   repeated targets on one unit through time need: a unit split across folds would put the same
#'   unit on both sides of a split and the held-out score would be read on rows the model had
#'   already seen a near-copy of. `NULL`, the default, is every row on its own.
#'
#' @return An integer vector of fold numbers named by unit, of class `timesift_folds`. Any named
#'   integer vector of the same shape is accepted wherever this is.
#'
#' @examples
#' y <- matrix(rbinom(300, 1, 0.3), nrow = 60,
#'             dimnames = list(sprintf("p%02d", 1:60), paste0("sp", 1:5)))
#' f <- fold_map(y, v = 5)
#' table(f)
#'
#' @export
fold_map <- function(y, v = 10L, seed = 1L, strata = 5L, by = NULL, group = NULL) {
  y <- .as_response(y)
  n <- nrow(y)
  value <- if (is.null(by)) rowSums(y) else as.numeric(by)
  if (length(value) != n) {
    stop("`by` must have one value per unit, got ", length(value), " for ", n, ".", call. = FALSE)
  }
  # A row on its own is a group of one, so grouping is the general case and there is one dealing
  # path rather than one per setting. A group's stratifying value is the mean of its rows'.
  key <- .group_key(group, n)
  .check_fold_count(v, key, grouped = !is.null(group))
  group_value <- as.numeric(tapply(value, key, mean))
  stratum <- if (strata <= 1L) rep(1L, max(key)) else .quantile_strata(group_value, strata)
  # The grouping travels with the map, so every split drawn inside a fold, the encoders'
  # validation set and the penalised fit's inner folds, keeps whole what the outer folds kept
  # whole.
  structure(stats::setNames(.seeded_deal(stratum, v, seed)[key], rownames(y)), v = as.integer(v),
            seed = seed,
            strata = as.integer(strata), grouped = !is.null(group),
            group = if (is.null(group)) NULL else as.character(group),
            class = "timesift_folds")
}

# The grouping a fold map carries, in the row order of `units`, or NULL where it carries none.
.fold_group <- function(folds, units) {
  # attr() partial-matches, and "group" is a prefix of "grouped".
  group <- attr(folds, "group", exact = TRUE)
  if (is.null(group)) {
    return(NULL)
  }
  stats::setNames(group, names(folds))[units]
}

#' How the folds are drawn
#'
#' The resampling [timesift()] scores on. `cv()` deals units into folds balanced on the
#' stratifying value; `grouped_cv()` deals whole groups, keeping every target sharing a group
#' value on one side of each split.
#'
#' `resampling` also accepts a fold vector or a [fold_map()] result directly, which is how a split
#' the package has no constructor for -- a spatial block, a season held out whole -- reaches the
#' same fitting path.
#'
#' @param v Number of folds.
#' @param seed Random seed, fixed so the map is reproducible.
#' @param strata Number of strata, or `1` for no stratification.
#' @param group The grouping: the name of a column of `targets`, or a vector with one value per
#'   target.
#'
#' @return A `timesift_resampling`.
#'
#' @examples
#' cv(v = 5L)
#' grouped_cv("site")
#'
#' @export
cv <- function(v = 10L, seed = 1L, strata = 5L) {
  .resampling("cv", v = v, seed = seed, strata = strata, group = NULL)
}

#' @rdname cv
#' @export
grouped_cv <- function(group, v = 10L, seed = 1L) {
  if (missing(group) || is.null(group)) {
    stop("`grouped_cv()` needs the grouping: a column of `targets`, or one value per target.",
         call. = FALSE)
  }
  .resampling("grouped_cv", v = v, seed = seed, strata = 1L, group = group)
}

.resampling <- function(method, v, seed, strata, group) {
  if (!is.numeric(v) || length(v) != 1L || v < 2L) {
    stop("`v` must be a fold count of 2 or more, got ", .describe(v), ".", call. = FALSE)
  }
  structure(list(method = method, v = as.integer(v), seed = seed, strata = as.integer(strata),
                 group = group),
            class = "timesift_resampling")
}

#' @export
print.timesift_resampling <- function(x, ...) {
  cat("<timesift resampling>", x$method, "in", .plural(x$v, "fold"), "\n")
  if (identical(x$method, "grouped_cv")) {
    cat("grouped by:", if (is.character(x$group) && length(x$group) == 1L) x$group else
      paste(.plural(length(unique(x$group)), "group"), "given as a vector"), "\n")
  } else {
    cat("strata    :", x$strata, "\n")
  }
  invisible(x)
}

# One fold map, however it was asked for: a resampling spec drawn against the response, or a split
# the caller brought, which reaches the same named integer vector everything downstream reads.
.as_fold_map <- function(resampling, y, targets, tf) {
  if (inherits(resampling, "timesift_resampling")) {
    return(fold_map(y, v = resampling$v, seed = resampling$seed, strata = resampling$strata,
                    group = .resampling_group(resampling, targets, tf)))
  }
  f <- .as_folds(.against_targets(resampling, tf), tf$label)
  if (length(unique(f)) < 2L) {
    stop("the fold map given as `resampling` holds one fold, so every unit is held out at once ",
         "and nothing is left to fit on. A fold map needs at least two folds.", call. = FALSE)
  }
  # A fold map the caller built carries what it knows about itself: reordered here to the
  # targets' order, and otherwise as it came, so a grouped design reports as one.
  if (inherits(resampling, "timesift_folds")) {
    return(structure(stats::setNames(f, tf$label), v = attr(resampling, "v"),
                     seed = attr(resampling, "seed"), strata = attr(resampling, "strata"),
                     grouped = isTRUE(attr(resampling, "grouped")),
                     group = unname(.fold_group(resampling, tf$label)),
                     class = "timesift_folds"))
  }
  structure(stats::setNames(f, tf$label), v = length(unique(f)), seed = NA,
            strata = NA_integer_, grouped = FALSE, class = "timesift_folds")
}

# A split without names was written against the targets as the caller passed them, and the targets
# have since been put in the order every array and the response carry, so it is read back into that
# order here. A named vector or a two-column table carries its own key and is joined on it.
.against_targets <- function(folds, tf) {
  if (is.data.frame(folds) || !is.null(names(folds)) || length(folds) != length(tf$order)) {
    return(folds)
  }
  folds[tf$order]
}

# The grouping is a column of the targets or a vector the caller wrote against them, and the
# targets have since been put in the order every array and the response carry.
.resampling_group <- function(resampling, targets, tf) {
  group <- resampling$group
  if (is.null(group)) {
    return(NULL)
  }
  if (is.character(group) && length(group) == 1L && group %in% names(targets)) {
    return(as.character(targets[[group]]))
  }
  if (length(group) != nrow(targets)) {
    stop("`grouped_cv()` was given ", length(group), " grouping value",
         if (length(group) > 1L) "s" else "", " for ", nrow(targets),
         " targets, and no column of `targets` is called \"",
         if (is.character(group)) group[1L] else class(group)[1L], "\".", call. = FALSE)
  }
  as.character(group)[tf$order]
}

#' @export
print.timesift_folds <- function(x, ...) {
  cat("<timesift folds>", .plural(length(x), "unit"), "in",
      .plural(attr(x, "v"), "fold"), "\n")
  print(table(fold = unclass(x)))
  invisible(x)
}

# Equal-count strata, with a value that fills more than one stratum's worth of the sample folded
# into a single stratum rather than split across boundaries it cannot be told apart on.
# Which group each row belongs to, numbered in order of first appearance; every row its own group
# where there is no grouping.
.group_key <- function(group, n) {
  if (is.null(group)) {
    return(seq_len(n))
  }
  if (length(group) != n) {
    stop("`group` must have one value per unit, got ", length(group), " for ", n, ".",
         call. = FALSE)
  }
  match(as.character(group), unique(as.character(group)))
}

.check_fold_count <- function(v, key, grouped) {
  n_group <- max(key)
  if (v < 2L || v > n_group) {
    stop("`v` must be between 2 and the ", n_group, if (grouped) " groups" else " units",
         ", got ", v, ".", call. = FALSE)
  }
  invisible(TRUE)
}

# The deal every fold map is made by, one label per stratified item under a fixed seed. The fold
# labels are permuted once, and the deal runs on from one stratum into the next: a counter
# restarted in every stratum hands the first labels one unit more than the last in every stratum,
# and a stratum smaller than `v` never reaches the last labels at all.
.seeded_deal <- function(stratum, v, seed) {
  old <- .seed_state()
  on.exit(.restore_seed(old), add = TRUE)
  set.seed(seed)
  labels <- sample.int(v)
  dealt <- integer(length(stratum))
  dealt_so_far <- 0L
  for (s in sort(unique(stratum))) {
    idx <- which(stratum == s)
    # Permute by position, never by value: sample() on a length-one vector samples 1:x instead of
    # returning x, so a stratum holding a single group would scatter one fold over every position
    # below that group's index and leave the map degenerate with nothing raised.
    dealt[idx[sample.int(length(idx))]] <- labels[(dealt_so_far + seq_along(idx) - 1L) %% v + 1L]
    dealt_so_far <- dealt_so_far + length(idx)
  }
  dealt
}

# Strata for a deal on one response: each distinct value its own stratum where there are no more
# than five, which is every presence-absence response and the group means of one, and quintiles
# otherwise. Quantile cuts cannot separate the two values of a presence-absence response and would
# leave it one stratum.
.response_strata <- function(value) {
  levels <- sort(unique(value))
  if (length(levels) <= 5L) match(value, levels) else .quantile_strata(value, 5L)
}

.quantile_strata <- function(value, k) {
  breaks <- unique(stats::quantile(value, probs = seq(0, 1, length.out = k + 1L), type = 7))
  if (length(breaks) < 3L) {
    return(rep(1L, length(value)))
  }
  as.integer(cut(value, breaks, include.lowest = TRUE, labels = FALSE))
}

# Seeding is a side effect on the session, so every entry point that seeds restores what was there.
.seed_state <- function() {
  if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) {
    get(".Random.seed", envir = globalenv(), inherits = FALSE)
  } else {
    NULL
  }
}

.restore_seed <- function(state) {
  if (is.null(state)) {
    suppressWarnings(rm(".Random.seed", envir = globalenv()))
  } else {
    assign(".Random.seed", state, envir = globalenv())
  }
  invisible(TRUE)
}

# A fold map reaches the fitting path as an integer vector in the row order of the representation,
# whether it arrived named, bare, or as a two-column table.
.as_folds <- function(folds, units) {
  if (is.data.frame(folds)) {
    if (ncol(folds) != 2L) {
      stop("a fold table needs two columns, a unit identifier and a fold.", call. = FALSE)
    }
    folds <- stats::setNames(as.integer(folds[[2L]]), as.character(folds[[1L]]))
  }
  if (!is.null(names(folds))) {
    missing <- setdiff(units, names(folds))
    if (length(missing)) {
      stop(length(missing), " unit", if (length(missing) > 1L) "s have" else " has",
           " no fold, first: ", missing[1L], ".", call. = FALSE)
    }
    folds <- folds[units]
  } else if (length(folds) != length(units)) {
    stop("an unnamed fold map must have one entry per unit, got ", length(folds),
         " for ", length(units), ".", call. = FALSE)
  }
  out <- as.integer(unname(folds))
  if (anyNA(out)) {
    stop("the fold map holds missing values.", call. = FALSE)
  }
  out
}
