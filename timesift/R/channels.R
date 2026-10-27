#' Where in the year, or the day, each bin sits
#'
#' An encoder that ends in global pooling discards when a thermal event happened, so the position
#' of a bin in the year has to be given to it as input if it is to be used at all. These channels
#' carry that position as the sine and cosine of the bin's fractional place in the year, which is
#' continuous across the turn of the year where the fraction itself is not. On a record read finer
#' than a day, the same pair for the place in the day carries where a reading sits in the daily
#' cycle.
#'
#' They are the time index of each bin, not a summary of the readings, so adding them introduces no
#' hand-built thermal feature: whatever a model does with them it could have done with a calendar.
#'
#' The position is read at the midpoint of the record each bin holds, in UTC, so a bin the record
#' only partly covers sits at the phase it was actually measured over. A site's longitude and a
#' zone's offset move the day's phase by the same amount for every bin, which a model absorbs, where
#' a local clock's summer time would move it twice a year. `inst/spec/representation.md` is the
#' normative description.
#'
#' @param x A [grain_matrix()] result. A [lookback_matrix()] result has no place in the calendar
#'   and is refused.
#' @param cycles Which cycles to place each bin in, `"year"`, `"day"` or both, in the order given.
#'   The day cycle reads bins that sit less than a day apart; at a day or coarser every bin would
#'   sit at the same place in the day, and it is refused.
#'
#' @return An array of the same units and bins with two channels per cycle, `year_sin` and
#'   `year_cos`, `day_sin` and `day_cos`, identical across units. Combine it with the readings
#'   using [bind_channels()]. The channels are recorded as positions in the `position` attribute,
#'   and the encoders of [torch_learners] read them at their own amplitude rather than
#'   standardise them.
#'
#' @examples
#' t <- seq(as.POSIXct("2021-09-01", tz = "UTC"), by = "hour", length.out = 24 * 400)
#' d <- data.frame(plot = "a", t = t, temp = sin(seq_along(t) / 500))
#' x <- grain_matrix(d, plot, t, temp, grain = "month")
#' round(calendar_channels(x)[1, 1:4, ], 3)
#'
#' hourly <- grain_matrix(d, plot, t, temp, grain = "native")
#' round(calendar_channels(hourly, cycles = c("year", "day"))[1, 1:4, ], 3)
#'
#' @export
calendar_channels <- function(x, cycles = "year") {
  .check_matrix(x)
  if (is.null(attr(x, "bin_start"))) {
    stop("a lookback's bins are placed relative to a target rather than on the calendar, so they ",
         "have no position in the year. `calendar_channels()` reads a grain_matrix().",
         call. = FALSE)
  }
  if (!is.character(cycles) || !length(cycles) || anyNA(cycles) || anyDuplicated(cycles)) {
    stop("`cycles` names each cycle once, from \"year\" and \"day\", got ", .describe(cycles), ".",
         call. = FALSE)
  }
  n_u <- dim(x)[1L]
  n_b <- dim(x)[2L]
  names <- as.vector(t(outer(cycles, c("sin", "cos"), paste, sep = "_")))
  out <- array(0, dim = c(n_u, n_b, length(names)),
               dimnames = list(dimnames(x)[[1L]], dimnames(x)[[2L]], names))
  for (k in seq_along(cycles)) {
    phase <- ts_cycle_phase_(as.numeric(attr(x, "bin_start")), as.numeric(attr(x, "bin_end")),
                             cycles[k])
    out[, , 2L * k - 1L] <- rep(phase[seq_len(n_b)], each = n_u)
    out[, , 2L * k] <- rep(phase[n_b + seq_len(n_b)], each = n_u)
  }
  out <- .carry_attrs(out, x, stats = names)
  attr(out, "position") <- names
  out
}

#' Put channels side by side
#'
#' Joins representations of the same units and bins into one array, in the order given. It is how
#' a temperature reading, an external product such as snow cover, and the calendar position of each
#' bin reach a model as one input.
#'
#' @param ... Two or more representations of shape `[unit, bin, channel]`, agreeing on their units
#'   and bins.
#'
#' @return One array carrying every channel, in the order the arguments are given and, inside each
#'   argument, in its own channel order. The attributes are the first argument's, with `stats` the
#'   joined names, except `static` and `position`, which name channels and so name those of every
#'   argument.
#'
#' @examples
#' t <- seq(as.POSIXct("2021-09-01", tz = "UTC"), by = "hour", length.out = 24 * 60)
#' d <- data.frame(plot = rep(c("a", "b"), each = length(t)), t = rep(t, 2),
#'                 temp = rnorm(2 * length(t)))
#' x <- grain_matrix(d, plot, t, temp, grain = "week")
#' dimnames(bind_channels(x, calendar_channels(x)))[[3]]
#'
#' @export
bind_channels <- function(...) {
  parts <- list(...)
  if (length(parts) < 2L) {
    stop("`bind_channels()` needs at least two representations.", call. = FALSE)
  }
  first <- parts[[1L]]
  for (k in seq_along(parts)) {
    if (!inherits(parts[[k]], "timesift_matrix")) {
      stop("argument ", k, " is a ", class(parts[[k]])[1L], ", not a representation.",
           call. = FALSE)
    }
    if (!identical(dimnames(parts[[k]])[1:2], dimnames(first)[1:2])) {
      stop("argument ", k, " covers different units or bins from the first.", call. = FALSE)
    }
  }
  names <- unlist(lapply(parts, function(p) dimnames(p)[[3L]]))
  if (anyDuplicated(names)) {
    stop("two representations carry a channel of the same name: ",
         paste(unique(names[duplicated(names)]), collapse = ", "), ".", call. = FALSE)
  }
  out <- array(unlist(lapply(parts, as.numeric), use.names = FALSE),
               dim = c(dim(first)[1:2], length(names)),
               dimnames = c(dimnames(first)[1:2], list(names)))
  out <- .carry_attrs(out, first, stats = names)
  for (a in c("static", "position")) {
    marked <- unlist(lapply(parts, attr, a))
    if (length(marked)) {
      attr(out, a) <- marked
    }
  }
  out
}

.check_matrix <- function(x) {
  if (!inherits(x, "timesift_matrix")) {
    stop("expected a grain_matrix() result, got ", class(x)[1L], ".", call. = FALSE)
  }
  invisible(TRUE)
}

.carry_attrs <- function(out, from, stats) {
  attr(out, "grain") <- attr(from, "grain")
  attr(out, "stats") <- stats
  attr(out, "year_start") <- attr(from, "year_start")
  attr(out, "bin_start") <- attr(from, "bin_start")
  attr(out, "bin_end") <- attr(from, "bin_end")
  attr(out, "bin_n") <- attr(from, "bin_n")
  attr(out, "bin_partial") <- attr(from, "bin_partial")
  class(out) <- c("timesift_matrix", "array")
  out
}
