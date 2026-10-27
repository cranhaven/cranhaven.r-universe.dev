#' Draw a ladder
#'
#' One line per learner across the grains, at the across-variable mean of the per-variable score,
#' with a 95 percent interval from its standard error across variables, on Student's t with one
#' degree of freedom fewer than there are variables. An open circle marks each learner's
#' best grain, which is where the curve says the record stops paying for being read more finely.
#'
#' @param x A [grain_ladder()] result.
#' @param col One colour per learner, recycled.
#' @param interval Draw the interval across variables.
#' @param ... Passed to [graphics::plot()].
#'
#' @return The summary table the plot is drawn from, invisibly.
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
#' x <- grain_matrix(d, plot, t, temp, grain = c("day", "week", "month"))
#' lad <- grain_ladder(x, y, elasticnet(), folds = fold_map(y, v = 3), verbose = FALSE)
#' plot(lad)
#'
#' @export
plot.timesift_ladder <- function(x, col = NULL, interval = TRUE, ...) {
  per_variable <- .per_variable(x)
  stat <- .curve_stats(per_variable$learner, per_variable$grain, per_variable$score,
                       arms = unique(x$learner), levels = unique(x$grain))
  .draw_curves(stat, col = col, interval = interval, xlab = "grain",
               ylab = attr(x, "metric"), ...)
  stat <- stat[c("arm", "level", "score", "se")]
  names(stat) <- c("learner", "grain", "score", "se")
  invisible(stat)
}

#' Draw a run
#'
#' One line per learner across the representations it ran on, read the way a ladder is read, and
#' the stack's held-out score drawn across them, its weights fitted inside each outer training
#' fold. The curves are scored on the folds a choice among them would be judged on, so the best of
#' them sits a little high; the ensemble line does not. Where the ensemble line sits above every
#' curve the candidates are carrying different parts of the signal, and where it sits on or below
#' the best curve they are not.
#'
#' @param x A `timesift` result.
#' @param col One colour per learner, recycled.
#' @param interval Draw the interval across responses.
#' @param ... Passed to [graphics::plot()].
#'
#' @return The table the plot is drawn from, invisibly.
#'
#' @export
plot.timesift <- function(x, col = NULL, interval = TRUE, ...) {
  per_response <- .per_response(x)
  if (!nrow(per_response)) {
    stop("this run scored no cell, so there is nothing to draw.", call. = FALSE)
  }
  stat <- .curve_stats(per_response$learner, per_response$representation, per_response$score,
                       arms = unique(per_response$learner),
                       levels = unique(per_response$representation))
  .draw_curves(stat, col = col, interval = interval, xlab = "representation",
               ylab = x$metric, rule = .ensemble_estimate(x), ...)
  stat <- stat[c("arm", "level", "score", "se")]
  names(stat) <- c("learner", "representation", "score", "se")
  invisible(stat)
}

# A level and the spread around it for every arm at every level, in the layout both plots draw
# from. One place, so a ladder and a run report the same quantity computed the same way.
.curve_stats <- function(arm, level, score, arms, levels) {
  out <- lapply(arms, function(a) {
    m <- vapply(levels, function(w) {
      v <- score[arm == a & level == w]
      c(.mean_se(v), sum(!is.na(v)))
    }, numeric(3L))
    data.frame(arm = a, level = levels, score = m[1L, ], se = m[2L, ],
               half = .t_margin(m[2L, ], m[3L, ]), stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, out)
  rownames(out) <- NULL
  out
}

.draw_curves <- function(stat, col, interval, xlab, ylab, rule = NULL, ...) {
  levels <- unique(stat$level)
  arms <- unique(stat$arm)
  if (is.null(col)) {
    col <- grDevices::hcl.colors(max(length(arms), 2L), "Dark 3")[seq_along(arms)]
  }
  col <- rep_len(col, length(arms))
  ruled <- !is.null(rule) && length(rule) == 1L && is.finite(rule)

  span <- if (interval && any(is.finite(stat$half))) {
    range(c(stat$score - stat$half, stat$score + stat$half), na.rm = TRUE)
  } else {
    range(stat$score, na.rm = TRUE)
  }
  span <- range(c(span, if (ruled) rule), na.rm = TRUE)
  args <- list(x = seq_along(levels), y = rep(NA_real_, length(levels)), ylim = span,
               xaxt = "n", xlab = xlab, ylab = ylab)
  do.call(graphics::plot, utils::modifyList(args, list(...)))
  graphics::axis(1L, at = seq_along(levels), labels = levels)
  graphics::grid(nx = NA, ny = NULL, col = "grey90", lty = 1L)
  if (ruled) {
    graphics::abline(h = rule, lty = 2L, col = "grey40", lwd = 2)
  }

  for (k in seq_along(arms)) {
    s <- stat[stat$arm == arms[k], , drop = FALSE]
    s <- s[match(levels, s$level), , drop = FALSE]
    at <- seq_along(levels)
    if (interval && any(is.finite(s$half) & s$half > 0)) {
      ok <- is.finite(s$half) & s$half > 0
      graphics::arrows(at[ok], (s$score - s$half)[ok], at[ok], (s$score + s$half)[ok],
                       length = 0.03, angle = 90, code = 3L, col = col[k])
    }
    graphics::lines(at, s$score, col = col[k], lwd = 2)
    graphics::points(at, s$score, col = col[k], pch = 19L)
    best <- which.max(s$score)
    graphics::points(at[best], s$score[best], col = col[k], pch = 1L, cex = 2.2, lwd = 2)
  }
  legend <- c(arms, if (ruled) "ensemble")
  if (length(legend) > 1L) {
    graphics::legend("bottomleft", legend = legend, col = c(col, if (ruled) "grey40"),
                     lty = c(rep(1L, length(arms)), if (ruled) 2L), lwd = 2, bty = "n")
  }
  invisible(stat)
}
