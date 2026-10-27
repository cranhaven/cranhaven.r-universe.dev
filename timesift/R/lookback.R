#' Reduce sensor series to a lookback anchored on each target
#'
#' Reads, for every target, a fixed length of record ending a fixed lag before that target's own
#' instant, and summarises it by one or more statistics. It is the reduction a calendar cannot
#' express: two targets on the same unit a fortnight apart read two different stretches of the same
#' series, so the bins are relative to the target rather than to a month or a week.
#'
#' @param data A data frame of readings in long form, one row per reading.
#' @param id Column identifying the unit carrying the sensor. A bare column name or a string.
#' @param time Column of reading instants, `POSIXct`. A bare column name or a string.
#' @param value Column of readings, numeric. A bare column name or a string.
#' @param at A data frame of targets with an `id` column naming the unit and an `at` column of
#'   anchor instants, `POSIXct`. One row per target; a unit may carry any number of them. The
#'   columns are read by name, as every alignment in the package is.
#' @param span The lookback's length, as a duration. See Durations.
#' @param lag The gap between the anchor and the end of the lookback, as a duration. Defaults to
#'   `"0 days"`, which ends the lookback at the anchor itself.
#' @param bins How many sub-bins the lookback is cut into, oldest first. `span` must divide by it
#'   exactly. One bin gives a block of features; several give a sequence a convolution can read.
#' @param stats Statistics to compute per bin, one channel each, in the order given. The same seven
#'   [grain_matrix()] carries.
#'
#' @details
#' Bin `b` of a target anchored at `a` covers `[a - lag - span + b * step, a - lag - span +
#' (b + 1) * step)`, with `step` the lookback's length divided by `bins` and `b` counted from zero.
#' The interval is closed at the left and open at the right, so a reading on a boundary belongs to
#' the later bin, and only the readings of the target's own unit are read.
#'
#' Every `(target, bin)` cell must hold at least one reading. A lookback reaching past either end of
#' the record is an error naming the target and the interval, never a padded row: an invented value
#' in front of a model is worse than a target the record cannot answer for.
#'
#' The four day-level statistics reduce each calendar day first, so they are defined only where
#' every day lies whole inside one bin. For a lookback that is two conditions rather than one:
#' `step` must be a whole number of days, and `a - lag - span` must fall on a day boundary. Either
#' failing is an error naming the target.
#'
#' @section Durations:
#' `span` and `lag` are read from a count and a unit -- `"30 days"`, `"12 hours"`, `"1 year"` --
#' or from a bare number of seconds. A **year is 365 days and a month is 30 days** here. A lookback
#' of a fixed length is a fixed length, not a calendar step: the point of anchoring on the target
#' is that every target reads the same amount of record, which a February and a leap year would
#' take away. Where the calendar is what matters, [grain_matrix()] is the call that follows it.
#'
#' The units are `seconds`, `minutes`, `hours`, `days`, `weeks`, `months` and `years`, singular or
#' plural.
#'
#' @section Time zone:
#' The calendar is the series', taken from the `tzone` attribute of `time` as [grain_matrix()]
#' takes it; a column with none is read as UTC. The anchors are instants and are read as a clock in
#' that same calendar, whatever zone `at` carries, so one record is binned by one calendar. The
#' span is measured on that clock: a lookback of one day ending at a local midnight holds the
#' whole local day before it, which is 25 hours of record on the night a zone sets its clock back
#' and 23 on the night it sets it forward. That is what keeps a calendar day whole inside a bin
#' for the four day-level statistics; a length fixed in instants could not.
#'
#' @return A numeric array of shape `[target, bin, channel]`, of class `timesift_matrix`. Its rows
#'   are the rows of `at`, in `at`'s own order, named by `at`'s row names. Its bins are named by
#'   where each one opens relative to the anchor, oldest first, and its channels by the statistic.
#'   Attributes:
#'   \itemize{
#'     \item `grain`: `"lookback"`.
#'     \item `span`, `lag`: the durations, resolved to seconds.
#'     \item `bins`: how many bins the lookback was cut into.
#'     \item `stats`: the statistic names in channel order.
#'     \item `bin_n`: a `[target, bin]` matrix of how many readings fell in each cell.
#'   }
#'
#' @seealso [grain_matrix()], the reduction that follows the calendar instead.
#'
#' @examples
#' t <- seq(as.POSIXct("2021-09-01", tz = "UTC"), by = "hour", length.out = 24 * 60)
#' d <- data.frame(plot = rep(c("a", "b"), each = length(t)),
#'                 t = rep(t, 2),
#'                 temp = c(sin(seq_along(t) / 24), cos(seq_along(t) / 24)))
#' at <- data.frame(id = c("a", "b"),
#'                  at = as.POSIXct(c("2021-10-20", "2021-10-25"), tz = "UTC"))
#' x <- lookback_matrix(d, plot, t, temp, at = at, span = "30 days", bins = 3L,
#'                    stats = c("cold_day", "mean", "warm_day"))
#' dim(x)
#' dimnames(x)[[2]]
#'
#' @export
lookback_matrix <- function(data,
                          id,
                          time,
                          value,
                          at,
                          span,
                          lag = "0 days",
                          bins = 1L,
                          stats = "mean") {
  id_col <- .resolve_column(substitute(id), data, parent.frame())
  time_col <- .resolve_column(substitute(time), data, parent.frame())
  value_col <- .resolve_column(substitute(value), data, parent.frame())

  span <- .parse_duration(span, "span")
  lag <- .parse_duration(lag, "lag")
  bins <- .check_bins(bins)
  stats <- .check_stats(stats, "lookback")

  unit <- .unit_names(data[[id_col]], id_col)
  when <- data[[time_col]]
  reading <- as.numeric(data[[value_col]])
  tz <- .series_zone(when, time_col)

  instant <- floor(as.numeric(when))
  .check_readings(unit, when, instant, id_col, time_col)
  local <- .naive_seconds(instant, tz, time_col)

  units <- sort(unique(unit), method = "radix")
  target <- .check_targets(at, units, tz)

  fit <- ts_reduce_lookbacks_(match(unit, units), reading, instant, local, units,
                            target$unit, target$at, target$label, span, lag, bins, stats)

  n_t <- length(target$label)
  out <- array(fit$values,
               dim = c(n_t, bins, length(stats)),
               dimnames = list(target$label, .bin_offsets(span, lag, bins), stats))

  attr(out, "grain") <- "lookback"
  attr(out, "span") <- span
  attr(out, "lag") <- lag
  attr(out, "bins") <- bins
  attr(out, "stats") <- stats
  attr(out, "bin_n") <- matrix(fit$bin_n, nrow = n_t, ncol = bins, dimnames = dimnames(out)[1:2])
  class(out) <- c("timesift_matrix", class(out))
  out
}

# A target is a unit and an instant, and its identity is its position in `at`: a unit may carry
# several targets, so the unit cannot name a row. The anchors are instants and go through the same
# boundary the readings do, so both are read as a clock in the series' own calendar.
.check_targets <- function(at, units, tz) {
  if (!is.data.frame(at) || !all(c("id", "at") %in% names(at))) {
    stop("`at` must be a data frame with an `id` column naming the unit and an `at` column of ",
         "anchor instants.", call. = FALSE)
  }
  if (!nrow(at)) {
    stop("`at` holds no target.", call. = FALSE)
  }
  who <- .unit_names(at$id, "id")
  anchor <- at$at
  if (!inherits(anchor, "POSIXct")) {
    stop("the `at` column of `at` must be POSIXct, not ", class(anchor)[1L], ".", call. = FALSE)
  }
  if (anyNA(who) || anyNA(anchor)) {
    stop("missing values in `at`. Fill or drop them before building a representation.",
         call. = FALSE)
  }
  index <- match(who, units)
  if (anyNA(index)) {
    n <- sum(is.na(index))
    stop(n, " target", if (n > 1L) "s" else "", " name a unit the series does not carry, first: ",
         who[which(is.na(index))[1L]], ".", call. = FALSE)
  }
  list(unit = index,
       at = .naive_seconds(floor(as.numeric(anchor)), tz),
       label = rownames(at))
}

.check_bins <- function(bins) {
  if (!is.numeric(bins) || length(bins) != 1L || !is.finite(bins) ||
        bins != floor(bins) || bins < 1) {
    stop("`bins` must be a positive whole number.", call. = FALSE)
  }
  as.integer(bins)
}

# A year is 365 days and a month is 30 days: a lookback of a fixed length is a fixed length, and
# every target has to read the same amount of record for their representations to be comparable.
.duration_seconds <- function() {
  c(second = 1, seconds = 1, minute = 60, minutes = 60, hour = 3600, hours = 3600,
    day = 86400, days = 86400, week = 604800, weeks = 604800, month = 2592000, months = 2592000,
    year = 31536000, years = 31536000)
}

.parse_duration <- function(x, arg) {
  if (is.numeric(x)) {
    if (length(x) != 1L || !is.finite(x) || x != floor(x)) {
      stop("`", arg, "` must be a whole number of seconds or a count and a unit, ",
           "like \"30 days\".", call. = FALSE)
    }
    return(as.numeric(x))
  }
  if (!is.character(x) || length(x) != 1L) {
    stop("`", arg, "` must be a whole number of seconds or a count and a unit, ",
         "like \"30 days\".", call. = FALSE)
  }
  parts <- regmatches(x, regexec("^ *([0-9]+) *([A-Za-z]*) *$", x))[[1L]]
  if (!length(parts)) {
    stop("`", arg, "` must be a count and a unit, like \"30 days\", got \"", x, "\".",
         call. = FALSE)
  }
  if (!nzchar(parts[3L])) {
    return(as.numeric(parts[2L]))
  }
  size <- .duration_seconds()[tolower(parts[3L])]
  if (is.na(size)) {
    stop("unknown duration unit \"", parts[3L], "\" in `", arg,
         "`. Available: seconds, minutes, hours, days, weeks, months, years.", call. = FALSE)
  }
  as.numeric(parts[2L]) * unname(size)
}

# The second dimension is the lookback itself, so a bin is named by where it opens relative to the
# anchor rather than by an instant no two targets share.
.bin_offsets <- function(span, lag, bins) {
  step <- span / bins
  vapply(seq_len(bins) - 1L, function(b) .format_duration(b * step - lag - span), character(1L))
}

.format_duration <- function(x) {
  scale <- c(day = 86400, hour = 3600, minute = 60, second = 1)
  size <- scale[which(x %% scale == 0)[1L]]
  n <- x / size
  paste0(n, " ", names(size), if (abs(n) == 1) "" else "s")
}
