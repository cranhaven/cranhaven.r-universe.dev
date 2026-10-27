#' How a series becomes an array a learner reads
#'
#' A representation is the reduction the package exists to make explicit: the record unreduced,
#' the record at a calendar grain, several grains bound into one block of features, or a lookback
#' of fixed length ending at each target's own instant. It carries the settings and nothing else,
#' so the same object describes a representation before any record has been seen, names the arm it
#' produced in a fitted object, and rebuilds itself for new targets in [predict.timesift()].
#'
#' @param grain One of `"native"`, `"halfday"`, `"day"`, `"week"`, `"month"`, `"season"`,
#'   `"year"`, or a function of the reading instants returning each reading's bin start. See
#'   [grain_matrix()].
#' @param grains Grains bound side by side into one block, or `NULL` for the automatic set
#'   [grains()] describes.
#' @param stats Statistics computed per bin, one channel each, in the order given. See
#'   [grain_matrix()] for the seven and for what separates an extreme reading from an extreme day.
#' @param year_start `"MM-DD"` boundary of the hydrological year, used by `"season"` and `"year"`.
#' @param span The lookback's length, as a duration such as `"30 days"` or a number of seconds.
#'   See [lookback_matrix()] for how a duration is read.
#' @param lag The gap between a target's instant and the end of its lookback.
#' @param bins Sub-bins the lookback is cut into, oldest first. One gives a block of features,
#'   several give a sequence.
#'
#' @details
#' Every representation carries `label`, the name it is reported under; `kind`, one of `"grain"`,
#' `"multigrain"` and `"lookback"`; the settings its kind uses; and `sequence`, which says whether
#' its bins are ordered in time and so mean something to a convolution. `native()`, `grain()` and a
#' `lookback()` of more than one bin are sequences; `multigrain()` and a one-bin `lookback()` are
#' blocks of features.
#'
#' `multigrain()` flattens each of its grains to one row per target and puts them side by side, so
#' a column of the block names the grain, the statistic and the bin it came from. It is the
#' tabular representation a penalised regression or a random forest reads.
#'
#' @return A `timesift_representation`.
#'
#' @examples
#' native()
#' grain("week", stats = c("cold_day", "mean", "warm_day"))
#' multigrain(c("month", "season"))
#' lookback("30 days", bins = 3L)
#'
#' @export
native <- function(stats = "mean", year_start = "09-01") {
  grain("native", stats = stats, year_start = year_start)
}

#' @rdname native
#' @export
grain <- function(grain, stats = "mean", year_start = "09-01") {
  g <- .check_grain(grain)
  if (!is.function(g) && length(g) != 1L) {
    stop("`grain()` takes one grain; name several with `grains()` or `multigrain()`.",
         call. = FALSE)
  }
  stats <- .check_stats(stats, g)
  .parse_year_start(year_start)
  .representation(if (is.function(g)) "custom" else g, "grain", stats = stats, sequence = TRUE,
                  grain = g, year_start = year_start)
}

#' @rdname native
#' @export
multigrain <- function(grains = NULL, stats = "mean", year_start = "09-01") {
  if (is.null(grains)) {
    .check_stats(stats, "day")
  } else {
    grains <- .check_grain(grains)
    if (is.function(grains)) {
      stop("`multigrain()` binds named grains; a supplied calendar is one grain, so pass it to ",
           "`grain()`.", call. = FALSE)
    }
    for (g in grains) .check_stats(stats, g)
  }
  .parse_year_start(year_start)
  .representation("multigrain", "multigrain", stats = stats, sequence = FALSE, grains = grains,
                  year_start = year_start)
}

#' @rdname native
#' @export
lookback <- function(span, lag = "0 days", bins = 1L, stats = "mean") {
  bins <- .check_bins(bins)
  stats <- .check_stats(stats, "lookback")
  seconds <- .parse_duration(span, "span")
  lag_seconds <- .parse_duration(lag, "lag")
  if (seconds <= 0) {
    stop("`span` must be a positive length of record.", call. = FALSE)
  }
  .representation(.lookback_label(span, lag, bins), "lookback", stats = stats, sequence = bins > 1L,
                  span = seconds, lag = lag_seconds, bins = bins)
}

.representation <- function(label, kind, stats, sequence, grain = NULL, grains = NULL,
                            span = NULL, lag = NULL, bins = NULL, year_start = NA_character_) {
  structure(list(label = label, kind = kind, grain = grain, grains = grains, stats = stats,
                 span = span, lag = lag, bins = bins, sequence = sequence,
                 year_start = year_start),
            class = "timesift_representation")
}

# A lookback is named by what distinguishes it from its neighbours in a set: two spans differ by
# their span, and a set that also varies the cut or the gap says so rather than clashing.
.lookback_label <- function(span, lag, bins) {
  out <- if (is.character(span)) span else .format_duration(.parse_duration(span, "span"))
  if (bins > 1L) {
    out <- paste0(out, " x", bins)
  }
  if (.parse_duration(lag, "lag") != 0) {
    out <- paste0(out, " lag ",
                  if (is.character(lag)) lag else .format_duration(.parse_duration(lag, "lag")))
  }
  out
}

#' @export
print.timesift_representation <- function(x, ...) {
  cat("<timesift representation>", x$label, "\n")
  cat("kind    :", x$kind, if (x$sequence) "(a sequence)" else "(a block of features)", "\n")
  if (identical(x$kind, "grain")) {
    cat("grain   :", if (is.function(x$grain)) "a supplied calendar" else x$grain, "\n")
  }
  if (identical(x$kind, "multigrain")) {
    cat("grains  :", if (is.null(x$grains)) "chosen from the record" else
      paste(x$grains, collapse = ", "), "\n")
  }
  if (identical(x$kind, "lookback")) {
    cat("span    :", .format_duration(x$span), "in", .plural(x$bins, "bin"),
        "ending", .format_duration(x$lag), "before the target\n")
  }
  cat("stats   :", paste(x$stats, collapse = ", "), "\n")
  invisible(x)
}

#' Several representations to run the same learners across
#'
#' The set a learner without a `data =` of its own is fitted at every member of. `grains()` names
#' calendar grains, `lookbacks()` names lookback spans, and either takes its arguments as separate
#' names or as one vector.
#'
#' `grains("auto")` is the set of named grains the record gives at least two bins, in the order
#' `native`, `halfday`, `day`, `week`, `month`, `season`, `year`, leaving out any grain the
#' requested statistics are not defined at. There is no cap on it: over three years of hourly
#' readings `native` is 26304 bins, and a caller who does not want that names the grains instead.
#'
#' @param ... Grain names for `grains()`, lookback spans for `lookbacks()`, given as separate
#'   arguments or as one vector.
#' @inheritParams native
#' @param x A named list of representations, a single representation, or a character vector of
#'   grain names.
#'
#' @return A `timesift_sift`: a named list of representations, labelled by each one's own label
#'   where the list was not named.
#'
#' @examples
#' grains("day", "week", "month")
#' grains(c("month", "year"), stats = c("cold_day", "mean", "warm_day"))
#' lookbacks("30 days", "90 days", bins = 3L)
#'
#' @export
grains <- function(..., stats = "mean", year_start = "09-01") {
  named <- unlist(list(...), use.names = FALSE)
  if (!length(named)) {
    stop("`grains()` needs at least one grain name, or \"auto\".", call. = FALSE)
  }
  if ("auto" %in% named) {
    if (length(named) > 1L) {
      stop("\"auto\" is the whole set the record supports and cannot be named beside a grain.",
           call. = FALSE)
    }
    .check_stats(stats, "day")
    .parse_year_start(year_start)
    return(structure(list(), class = "timesift_sift", auto = TRUE, stats = stats,
                     year_start = year_start))
  }
  as_sift(lapply(named, grain, stats = stats, year_start = year_start))
}

#' @rdname grains
#' @export
lookbacks <- function(..., lag = "0 days", bins = 1L, stats = "mean") {
  spans <- unlist(list(...), use.names = FALSE)
  if (!length(spans)) {
    stop("`lookbacks()` needs at least one span, such as \"30 days\".", call. = FALSE)
  }
  as_sift(lapply(spans, lookback, lag = lag, bins = bins, stats = stats))
}

#' @rdname grains
#' @export
as_sift <- function(x) {
  if (inherits(x, "timesift_sift")) {
    return(x)
  }
  if (inherits(x, "timesift_representation")) {
    x <- list(x)
  }
  if (is.character(x)) {
    return(grains(x))
  }
  if (!is.list(x) || !length(x)) {
    stop("a sift is a non-empty list of representations, or a vector of grain names.",
         call. = FALSE)
  }
  ok <- vapply(x, inherits, logical(1L), "timesift_representation")
  if (!all(ok)) {
    stop("element", if (sum(!ok) > 1L) "s" else "", " ", .listing(which(!ok)), " ",
         if (sum(!ok) > 1L) "are" else "is",
         " not a representation from native(), grain(), multigrain() or lookback().", call. = FALSE)
  }
  auto <- vapply(x, function(r) r$label, character(1L))
  given <- names(x)
  names(x) <- if (is.null(given)) auto else ifelse(nzchar(given), given, auto)
  if (anyDuplicated(names(x))) {
    stop("two representations are reported under the same name: ",
         .listing(unique(names(x)[duplicated(names(x))])),
         ". Name the list to tell them apart.", call. = FALSE)
  }
  structure(x, class = "timesift_sift")
}

#' @rdname combine
#' @export
c.timesift_representation <- function(...) {
  .sift_of(list(...))
}

#' @rdname combine
#' @export
c.timesift_sift <- function(...) {
  .sift_of(list(...))
}

# `grains("auto")` stands for every grain the record turns out to carry, which is not known until
# a record is read. Combining it with named ones would have to mean either "these as well" or
# "these instead", so it is refused rather than resolved to one of them.
.sift_of <- function(args) {
  if (any(vapply(args, function(a) isTRUE(attr(a, "auto")), logical(1L)))) {
    stop("grains(\"auto\") is every grain the record carries, so it cannot be combined with ",
         "others. Name the grains you want instead.", call. = FALSE)
  }
  as_sift(.splice(args, "timesift_representation"))
}

#' @export
print.timesift_sift <- function(x, ...) {
  if (isTRUE(attr(x, "auto"))) {
    cat("<timesift sift> every grain the record carries, statistics:",
        paste(attr(x, "stats"), collapse = ", "), "\n")
    return(invisible(x))
  }
  cat("<timesift sift>", .plural(length(x), "representation"), "\n")
  for (nm in names(x)) {
    r <- x[[nm]]
    cat(sprintf("  %-14s %-11s %s\n", nm, r$kind, paste(r$stats, collapse = ", ")))
  }
  invisible(x)
}

#' @export
`[.timesift_sift` <- function(x, i) {
  as_sift(NextMethod())
}

#' Build one representation for a set of targets
#'
#' Turns a representation and the two tables into the `[target, bin, channel]` array a learner is
#' fitted on. It is the one place the fitting layer builds an array, so [timesift()] and
#' [predict.timesift()] reach a record the same way and a candidate refitted on new targets is
#' built from the settings its own arm was.
#'
#' The rows are the targets, in the order the fitting layer keeps them: sorted by identifier where
#' one target row belongs to each unit, and in the targets' own order where `target_time` anchors
#' them. Columns named in `static` are appended as channels holding one value per target, constant
#' across the bins.
#'
#' @param rep A representation from [native()], [grain()], [multigrain()] or [lookback()].
#' @param series The long table of readings, or `NULL` where the targets carry the whole predictor
#'   block.
#' @param targets The table of prediction targets.
#' @param spec The resolved settings [timesift()] carries: the identifier, time, value, anchor and
#'   static columns it settled on.
#'
#' @return A `timesift_matrix`.
#'
#' @export
build_representation <- function(rep, series, targets, spec) {
  if (!inherits(rep, "timesift_representation")) {
    stop("expected a representation from native(), grain(), multigrain() or lookback(), got ",
         class(rep)[1L], ".", call. = FALSE)
  }
  tf <- .target_frame(targets, spec)
  ordered <- targets[tf$order, , drop = FALSE]
  if (identical(rep$kind, "static")) {
    return(feature_matrix(.static_matrix(ordered, tf, spec), label = rep$label))
  }
  if (is.null(series)) {
    stop("`", rep$label, "` reads the series, and none was given.", call. = FALSE)
  }
  x <- switch(rep$kind,
    grain = .grain_block(rep, series, tf, spec),
    multigrain = .multigrain_block(rep, series, tf, spec),
    lookback = .lookback_block(rep, series, tf, spec),
    stop("unknown representation kind \"", rep$kind, "\".", call. = FALSE))
  .append_static(x, .static_matrix(ordered, tf, spec))
}

# The predictor block a targets-only fit is made of. It is a representation like any other so that
# the candidate, the score and the refit above it need no second shape.
.static_representation <- function() {
  .representation("static", "static", stats = "static", sequence = FALSE)
}

# Which targets there are, what they are called, and in which order every array and the response
# carry them. Sorting by identifier is what makes a row of the response line up with the row of a
# grain representation, whose units the core returns sorted; an anchored fit keeps the targets'
# own order, which is the order `lookback_matrix()` returns.
.target_frame <- function(targets, spec) {
  n <- nrow(targets)
  if (!n) {
    stop("`targets` holds no row.", call. = FALSE)
  }
  id <- if (is.null(spec$id)) NULL else .unit_names(targets[[spec$id]], spec$id)
  rn <- attr(targets, "row.names")
  # A target row is named by its unit where it is the only one that unit has, because that is what
  # the row of a calendar representation is named by; an anchored row is named by its position,
  # because its unit names several rows.
  label <- if (!is.null(id) && is.null(spec$target_time)) {
    id
  } else if (is.character(rn)) {
    rn
  } else {
    as.character(seq_len(n))
  }
  ord <- if (!is.null(id) && is.null(spec$target_time)) {
    order(id, method = "radix")
  } else {
    seq_len(n)
  }
  at <- if (is.null(spec$target_time)) NULL else targets[[spec$target_time]]
  list(label = label[ord], id = id[ord], at = at[ord], order = ord)
}

.grain_block <- function(rep, series, tf, spec) {
  parts <- lapply(spec$value, function(v) {
    m <- grain_matrix(series, id = spec$id, time = spec$time, value = v, grain = rep$grain,
                      stats = rep$stats, year_start = rep$year_start, partial = spec$partial)
    if (length(spec$value) > 1L) .prefix_channels(m, v) else m
  })
  x <- if (length(parts) == 1L) parts[[1L]] else do.call(bind_channels, parts)
  .align_targets(x, tf$label)
}

# What a representation anchored on the target needs, said in one place: the run raises it before
# anything is fitted, and the builder raises it at the one door that reaches a lookback without
# going through that check.
.needs_target_time <- function(labels) {
  stop(.listing(unique(labels)), " reads a stretch of record ending at each target's own instant, ",
       "so `target_time` has to name the column of `targets` holding it.", call. = FALSE)
}

.lookback_block <- function(rep, series, tf, spec) {
  if (is.null(tf$at)) {
    .needs_target_time(rep$label)
  }
  at <- data.frame(id = tf$id, at = tf$at, stringsAsFactors = FALSE)
  rownames(at) <- tf$label
  parts <- lapply(spec$value, function(v) {
    m <- lookback_matrix(series, id = spec$id, time = spec$time, value = v, at = at,
                       span = rep$span, lag = rep$lag, bins = rep$bins, stats = rep$stats)
    if (length(spec$value) > 1L) .prefix_channels(m, v) else m
  })
  if (length(parts) == 1L) parts[[1L]] else do.call(bind_channels, parts)
}

.multigrain_block <- function(rep, series, tf, spec) {
  built <- if (is.null(rep$grains)) {
    .auto_grains(rep$stats, rep$year_start, series, tf, spec)
  } else {
    stats::setNames(lapply(rep$grains, function(g) {
      .grain_block(grain(g, stats = rep$stats, year_start = rep$year_start), series, tf, spec)
    }), rep$grains)
  }
  blocks <- lapply(names(built), function(g) {
    f <- .flatten(built[[g]])
    colnames(f) <- paste0(g, ":", colnames(f))
    f
  })
  feature_matrix(do.call(cbind, blocks), label = rep$label)
}

# The grains the record actually carries, built once and handed back so that the caller that asked
# which they are is also the caller that has them. A grain the requested statistics are not defined
# at is not a candidate at all, which is what `.check_stats()` decides.
.auto_grains <- function(stats, year_start, series, tf, spec) {
  out <- list()
  for (g in .grains()) {
    if (is.null(tryCatch(.check_stats(stats, g), error = function(e) NULL))) {
      next
    }
    m <- .grain_block(grain(g, stats = stats, year_start = year_start), series, tf, spec)
    if (dim(m)[2L] >= 2L) {
      out[[g]] <- m
    }
  }
  if (!length(out)) {
    stop("no grain gives the record two bins, so there is nothing to compare. The record spans ",
         "less than two of every calendar grain.", call. = FALSE)
  }
  out
}

# A target reads its own unit's row of a grain representation, whose rows are every unit the
# series carries. Two targets on one unit is the anchored case and never reaches here.
.align_targets <- function(x, labels) {
  idx <- match(labels, dimnames(x)[[1L]])
  if (anyNA(idx)) {
    missing <- labels[is.na(idx)]
    stop(.plural(length(missing), "target"), " name a unit the series does not carry: ",
         .listing(missing), ".", call. = FALSE)
  }
  .subset_units(x, idx)
}

# One channel per value column per statistic, so a series carrying temperature and snow reaches a
# model as channels that say which is which.
.prefix_channels <- function(m, value) {
  nm <- paste(value, dimnames(m)[[3L]], sep = "_")
  dimnames(m)[[3L]] <- nm
  attr(m, "stats") <- nm
  m
}

.static_matrix <- function(ordered, tf, spec) {
  cols <- spec$static
  out <- matrix(numeric(0), nrow = nrow(ordered), ncol = 0L,
                dimnames = list(tf$label, character(0)))
  if (!length(cols)) {
    return(out)
  }
  # Named before typed: a column that is not there reads as not numeric, and the message would
  # send the reader to encode a column their table does not carry. This is the predicting path,
  # where the targets are a new frame that has to hold what the fit was made with.
  gone <- setdiff(cols, names(ordered))
  if (length(gone)) {
    stop("`targets` does not carry the static predictor", if (length(gone) > 1L) "s" else "",
         " ", .listing(gone), " the fit was made with.", call. = FALSE)
  }
  bad <- cols[!vapply(cols, function(v) is.numeric(ordered[[v]]), logical(1L))]
  if (length(bad)) {
    stop("`static` column", if (length(bad) > 1L) "s" else "", " ", .listing(bad), " ",
         if (length(bad) > 1L) "are" else "is",
         " not numeric. Encode ", if (length(bad) > 1L) "them" else "it",
         " as numbers before fitting.", call. = FALSE)
  }
  out <- as.matrix(ordered[, cols, drop = FALSE])
  storage.mode(out) <- "double"
  if (anyNA(out)) {
    stop("`static` holds missing values. Fill or drop them before fitting.", call. = FALSE)
  }
  dimnames(out) <- list(tf$label, cols)
  out
}

# A static predictor is one number per target and the array is one number per target, bin and
# channel, so it enters as a channel that does not move across the bins: the constant an encoder
# reads beside the readings. Which channels those are is recorded, because a learner reading the
# bins as a block of features wants one predictor from each of them rather than one per bin.
.append_static <- function(x, static) {
  if (!ncol(static)) {
    return(x)
  }
  d <- dim(x)
  block <- array(NA_real_, dim = c(d[1L], d[2L], ncol(static)),
                 dimnames = list(dimnames(x)[[1L]], dimnames(x)[[2L]], colnames(static)))
  for (j in seq_len(ncol(static))) {
    block[, , j] <- static[, j]
  }
  out <- bind_channels(x, .carry_attrs(block, x, stats = colnames(static)))
  # A lookback carries its span, its lag and its cut, which the calendar path has no equivalent of
  # and the channel join does not know to keep.
  for (a in setdiff(names(attributes(x)), names(attributes(out)))) {
    attr(out, a) <- attr(x, a)
  }
  attr(out, "static") <- c(attr(x, "static"), colnames(static))
  out
}
