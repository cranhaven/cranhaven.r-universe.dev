#' Reduce sensor series to a temporal grain
#'
#' Bins each unit's readings by the calendar and summarises every bin by one or more statistics,
#' returning the array a model is fitted on. The reduction is the choice the package exists to
#' make explicit: `grain` sets how coarse the record becomes, `stats` sets what survives the
#' reduction, and the two are not interchangeable.
#'
#' @param data A data frame of readings in long form, one row per reading.
#' @param id Column identifying the unit carrying the sensor. A bare column name or a string.
#' @param time Column of reading instants, `POSIXct`. A bare column name or a string.
#' @param value Column of readings, numeric. A bare column name or a string.
#' @param grain One of `"native"`, `"halfday"`, `"day"`, `"week"`, `"month"`, `"season"`,
#'   `"year"`. The four coarse grains follow the calendar, so a bin is a real week or month
#'   rather than a fixed block of hours. Naming several grains returns one representation per
#'   grain, a [timesift_set()]. A function is called on the reading instants and must return
#'   the `POSIXct` start of each reading's bin, which is how a calendar the package does not
#'   carry, such as astronomical seasons, is binned.
#' @param stats Statistics to compute per bin, one channel each, in the order given. See Details.
#' @param year_start `"MM-DD"` boundary of the hydrological year, used by `"season"` and
#'   `"year"`. Defaults to `"09-01"`.
#' @param partial What to do with a bin the record does not cover for its whole calendar span,
#'   which is what a record beginning or ending away from a bin boundary produces. `"keep"`, the
#'   default, returns it alongside the full bins; `"drop"` removes it. See Partial bins.
#'
#' @details
#' Seven statistics are available, and the distinction between an extreme reading, an extreme day
#' and a typical day is deliberate rather than pedantic:
#'
#' \itemize{
#'   \item `mean`: arithmetic mean of the readings in the bin.
#'   \item `min`, `max`: coldest and warmest single reading in the bin.
#'   \item `cold_day`, `warm_day`: coldest and warmest day, each day first reduced to its own
#'     mean. Defined for `"day"` and coarser.
#'   \item `mean_daily_min`, `mean_daily_max`: the bin's average daily minimum and average daily
#'     maximum, each day first reduced to its own extreme. Defined for `"day"` and coarser.
#' }
#'
#' An extreme day is a state the unit was in; an extreme reading can be one hour; an average daily
#' extreme is the exposure a typical day of the bin brought. On alpine soil temperature the
#' day-level pair carries more predictive signal than the bin mean, and by more the coarser the
#' bin.
#'
#' Whether a grain is a day or coarser is decided from the bins rather than from the grain's
#' name, so a supplied calendar that cuts inside a day is refused for these four as well, naming
#' the day it splits.
#'
#' Nothing is standardised here. Scaling belongs to the fold it is computed on, never to the
#' representation, because computing it over all units would leak held-out units into the input.
#'
#' @section Time zone:
#' Bins follow the calendar the series is carried in, which is the `tzone` attribute of `time`;
#' a column with none is read as UTC, and a name the zone database does not know is an error. The
#' zone is resolved once, at the edge: below it the binning works in local time, where a day is
#' 86400 seconds whatever the night did, so a zone that moves its clock at midnight has no
#' midnight to lose. A bin start is a local time, so reporting it back as an instant needs a rule:
#' one the clock skipped resolves to the instant the clock jumped to, one the clock repeated to
#' the first of the two. Instants are read at whole seconds.
#'
#' In a zone that keeps daylight saving time a day is therefore a wall-clock day: the day the clock
#' goes forward spans 23 hours and the day it goes back 25, and the week and the month holding
#' them one hour less or more. Hourly readings carried in `"Europe/Vienna"` give a 2021-03-28 day
#' of 23 readings and a 2021-10-31 day of 25. A record kept on a fixed offset, as the Schrankogel
#' deposit is (26,304 hourly readings, 1,096 days of 24), bins into days of 24 hours; a series
#' carried in a daylight-saving zone bins that way too once its `tzone` is set to the fixed
#' offset of standard time, such as `"Etc/GMT-1"` for Central European Time, whose days then run
#' from 01:00 to 01:00 on the summer clock.
#'
#' The `"native"` grain is the one grain not read on that clock: its bin is the reading itself,
#' so the two readings of an hour a zone repeats are two bins, and the record read at `"native"`
#' is the same array whichever zone it is carried in.
#'
#' @section Bins that do not tile the record:
#' Every unit must reach every bin, and consecutive bins must be one bin apart on the grain's own
#' calendar. A bin no unit reaches is never built, so a month missing from the whole record would
#' otherwise pass as four adjacent monthly bins with one simply gone. Neither the `"native"` grain,
#' whose bin is the reading itself, nor a supplied calendar, which declares its own bin lengths,
#' is held to the second rule. [coverage()] lays the same binning out as a count of readings per
#' unit and bin, which is where a refused record's gaps are read off.
#'
#' @section Partial bins:
#' A bin is partial when the record does not cover its whole calendar span. Which bins those are
#' follows from where the record starts and stops against the calendar, not from the grain alone:
#' three years of hourly readings from 1 September carry no partial month and no partial season on
#' a `"09-01"` boundary, but the same record carries a partial week at each end, because 1
#' September is a Wednesday. A record from an arbitrary deployment date carries one at each end of
#' almost every grain.
#'
#' A bin is partial if its start precedes the first reading of the record, or if its calendar span
#' runs past the last reading plus the record's own sampling interval, taken as the smallest gap
#' between consecutive distinct reading instants. Only a bin at an end of the record can satisfy
#' either, because every unit is required to span every bin in between. The verdict is returned as
#' the `bin_partial` attribute whichever way `partial` is set, so a kept partial bin is labelled
#' rather than silent.
#'
#' Keeping partial bins is the default because dropping them discards the record's ends: on a
#' seasonal grain that is up to three months of readings at each end. The cost of keeping them is
#' that such a bin's mean is taken over fewer readings and its extremes over fewer days, so
#' `cold_day` and `warm_day` there are drawn from a shorter draw and sit closer to the bin mean
#' than a full bin's would. `bin_n` gives the count the bin was actually reduced from.
#'
#' A caller-supplied binning declares its own bins, so the package cannot know where the last one
#' was meant to end and takes the record's end as its end. Such a final bin is never reported
#' partial; its leading bin is judged as any other.
#'
#' @return A numeric array of shape `[unit, bin, channel]`, with dimnames giving the sorted unit
#'   identifiers, the ISO-8601 start of each bin, and the statistic names. Attributes:
#'   \itemize{
#'     \item `grain`: the grain name.
#'     \item `stats`: the statistic names in channel order.
#'     \item `year_start`: the boundary used.
#'     \item `bin_start`: the instant each bin begins on the calendar, which is earlier
#'       than the bin's first reading wherever the record does not reach the boundary.
#'     \item `bin_end`: the last reading instant assigned to each bin.
#'     \item `bin_n`: a `[unit, bin]` matrix of how many readings fell in each bin.
#'     \item `bin_partial`: a logical vector marking the bins the record does not cover for their
#'       whole calendar span.
#'   }
#'   Naming more than one grain returns a [timesift_set()] of those arrays.
#'
#' @examples
#' t <- seq(as.POSIXct("2021-09-01", tz = "UTC"), by = "hour", length.out = 24 * 40)
#' d <- data.frame(plot = rep(c("a", "b"), each = length(t)),
#'                 t = rep(t, 2),
#'                 temp = c(sin(seq_along(t) / 24), cos(seq_along(t) / 24)))
#' x <- grain_matrix(d, plot, t, temp, grain = "week",
#'                    stats = c("cold_day", "mean", "warm_day"))
#' dim(x)
#' dimnames(x)[[3]]
#'
#' ladder <- grain_matrix(d, plot, t, temp, grain = c("day", "week"))
#' names(ladder)
#'
#' @export
grain_matrix <- function(data,
                          id,
                          time,
                          value,
                          grain = "day",
                          stats = "mean",
                          year_start = "09-01",
                          partial = c("keep", "drop")) {
  id_col <- .resolve_column(substitute(id), data, parent.frame())
  time_col <- .resolve_column(substitute(time), data, parent.frame())
  value_col <- .resolve_column(substitute(value), data, parent.frame())

  partial <- match.arg(partial)
  grain <- .check_grain(grain)
  if (length(grain) > 1L) {
    out <- lapply(grain, function(w) {
      grain_matrix(data, id = id_col, time = time_col, value = value_col,
                    grain = w, stats = stats, year_start = year_start, partial = partial)
    })
    return(timesift_set(stats::setNames(out, grain)))
  }

  stats <- .check_stats(stats, grain)
  ys <- .parse_year_start(year_start)

  unit <- .unit_names(data[[id_col]], id_col)
  when <- data[[time_col]]
  reading <- as.numeric(data[[value_col]])
  tz <- .series_zone(when, time_col)

  # Instants at second resolution, and the same instants read as a clock in the series' own zone.
  # The zone is resolved here and nowhere below it: the core bins a calendar with no zone in it, so
  # a day there is 86400 seconds of local time whatever the night did.
  instant <- floor(as.numeric(when))
  .check_readings(unit, when, instant, id_col, time_col)
  local <- .naive_seconds(instant, tz, time_col)

  # C collation, never the session's, so the row order of the representation is the same on
  # every machine and the same as the one NumPy gives the Python side. R's default sort
  # follows LC_COLLATE, which orders `P10` against `P9` and `a` against `A` by rules that
  # differ between locales, and a response matrix built in one order against a
  # representation built in the other lines up row for row while naming different units.
  units <- sort(unique(unit), method = "radix")
  supplied <- if (is.function(grain)) .custom_bins(grain, when, tz, unit) else NULL

  fit <- ts_reduce_(match(unit, units), reading, instant, local, supplied, units,
                    if (is.function(grain)) "custom" else grain,
                    ys$month, ys$day, stats, .sampling_step(instant))

  bins <- .bin_instants(fit$bin_start, grain, tz)
  n_u <- length(units)
  n_b <- length(bins)
  out <- array(fit$values,
               dim = c(n_u, n_b, length(stats)),
               dimnames = list(units, .iso_instant(bins), stats))

  attr(out, "grain") <- if (is.function(grain)) "custom" else grain
  attr(out, "stats") <- stats
  attr(out, "year_start") <- year_start
  attr(out, "bin_start") <- bins
  attr(out, "bin_end") <- .POSIXct(fit$bin_end, tz = tz)
  attr(out, "bin_n") <- matrix(fit$bin_n, nrow = n_u, ncol = n_b, dimnames = dimnames(out)[1:2])
  attr(out, "bin_partial") <- fit$bin_partial
  class(out) <- c("timesift_matrix", class(out))
  if (partial == "drop") .drop_partial(out) else out
}

# Keeping or dropping a partial bin is the caller's choice, so the array is built over every bin
# the calendar produced and the unwanted ones are removed afterwards, which keeps one binning path
# rather than one per setting.
.drop_partial <- function(x) {
  keep <- which(!attr(x, "bin_partial"))
  if (!length(keep)) {
    stop("dropping the partial bins leaves no bin: the record covers no whole ",
         attr(x, "grain"), ". Use `partial = \"keep\"` or a finer grain.", call. = FALSE)
  }
  if (length(keep) == dim(x)[2L]) {
    return(x)
  }
  out <- x[, keep, , drop = FALSE]
  dimnames(out) <- list(dimnames(x)[[1L]], dimnames(x)[[2L]][keep], dimnames(x)[[3L]])
  for (a in c("grain", "stats", "year_start")) {
    attr(out, a) <- attr(x, a)
  }
  attr(out, "bin_start") <- attr(x, "bin_start")[keep]
  attr(out, "bin_end") <- attr(x, "bin_end")[keep]
  attr(out, "bin_n") <- attr(x, "bin_n")[, keep, drop = FALSE]
  attr(out, "bin_partial") <- attr(x, "bin_partial")[keep]
  class(out) <- c("timesift_matrix", "array")
  out
}

#' @export
print.timesift_matrix <- function(x, ...) {
  d <- dim(x)
  cat("<timesift matrix>", .plural(d[1L], "unit"), "x", .plural(d[2L], "bin"),
      "x", .plural(d[3L], "channel"), "\n")
  cat("grain:", attr(x, "grain"), "  stats:", paste(attr(x, "stats"), collapse = ", "), "\n")
  span <- attr(x, "bin_start")
  if (!all(is.na(span))) {
    cat("from  :", format(min(span)), "to", format(max(span)), "\n")
  }
  invisible(x)
}

.plural <- function(n, word) {
  paste0(n, " ", word, if (n == 1L) "" else "s")
}

.grains <- function() {
  c("native", "halfday", "day", "week", "month", "season", "year")
}

.day_level_stats <- function() {
  c("cold_day", "warm_day", "mean_daily_min", "mean_daily_max")
}

.known_stats <- function() {
  c("mean", "min", "max", .day_level_stats())
}

.check_grain <- function(grain) {
  if (is.function(grain)) {
    return(grain)
  }
  if (!is.character(grain) || !length(grain)) {
    stop("`grain` must be a grain name, a vector of them, or a function.", call. = FALSE)
  }
  bad <- setdiff(grain, .grains())
  if (length(bad)) {
    stop("unknown grain: ", paste(bad, collapse = ", "),
         ". Available: ", paste(.grains(), collapse = ", "), ".", call. = FALSE)
  }
  if (anyDuplicated(grain)) {
    stop("`grain` names a grain twice: ",
         paste(unique(grain[duplicated(grain)]), collapse = ", "), ".", call. = FALSE)
  }
  grain
}

.resolve_column <- function(expr, data, envir) {
  name <- if (is.character(expr) && length(expr) == 1L) expr else deparse(expr)
  if (!name %in% names(data)) {
    evaluated <- tryCatch(eval(expr, envir), error = function(e) NULL)
    if (is.character(evaluated) && length(evaluated) == 1L && evaluated %in% names(data)) {
      name <- evaluated
    } else {
      stop("column `", name, "` is not in the data.", call. = FALSE)
    }
  }
  name
}

.check_stats <- function(stats, grain) {
  known <- .known_stats()
  bad <- setdiff(stats, known)
  if (length(bad)) {
    stop("unknown statistic: ", paste(bad, collapse = ", "),
         ". Available: ", paste(known, collapse = ", "), ".", call. = FALSE)
  }
  if (anyDuplicated(stats)) {
    stop("`stats` names a statistic twice: ",
         paste(unique(stats[duplicated(stats)]), collapse = ", "), ".", call. = FALSE)
  }
  if (is.character(grain) && grain %in% c("native", "halfday") &&
        any(stats %in% .day_level_stats())) {
    stop("`", grain, "` bins are shorter than a day, so ",
         paste(intersect(stats, .day_level_stats()), collapse = " and "),
         " is not defined there. Use a grain of `day` or coarser.", call. = FALSE)
  }
  stats
}

.parse_year_start <- function(year_start) {
  if (!grepl("^[0-9]{2}-[0-9]{2}$", year_start)) {
    stop("`year_start` must look like \"MM-DD\", got \"", year_start, "\".", call. = FALSE)
  }
  parts <- as.integer(strsplit(year_start, "-", fixed = TRUE)[[1L]])
  if (parts[1L] < 1L || parts[1L] > 12L || parts[2L] < 1L || parts[2L] > 28L) {
    stop("`year_start` must be a month 01-12 and a day 01-28, got \"", year_start, "\".",
         call. = FALSE)
  }
  list(month = parts[1L], day = parts[2L])
}

# An identifier is a name, and every place one decides a position it is written by one rule, here.
# A character id is itself and a factor is its label; a whole number is its digits, with no
# exponent and no decimal point, so an id read as 100000 from a file is `100000` rather than R's
# `1e+05` beside Python's `100000.0`. A number that is not whole has no such writing and is
# refused, as is a column of any other type: a response aligned by name against a representation
# built in the other language would otherwise match none of its units.
.unit_names <- function(x, column) {
  if (is.factor(x)) {
    return(as.character(x))
  }
  if (is.character(x)) {
    return(x)
  }
  if (is.numeric(x)) {
    bad <- !is.na(x) & (!is.finite(x) | x != floor(x))
    if (any(bad)) {
      stop("`", column, "` identifies a unit by ", format(x[which(bad)[1L]]),
           ", which is not a whole number. An identifier is a name; round it or write it as ",
           "text before naming it.", call. = FALSE)
    }
    out <- rep(NA_character_, length(x))
    kept <- !is.na(x)
    out[kept] <- sub("^-0$", "0", sprintf("%.0f", x[kept]))
    return(out)
  }
  stop("`", column, "` must identify a unit by text, a factor level or a whole number, not ",
       class(x)[1L], ".", call. = FALSE)
}

# The readings the core cannot see for itself: it is handed unit indices and whole seconds, so a
# hole in the id or the time column has already become something else by the time it gets there. A
# reading's own value is the core's guard, raised once for both languages.
.check_readings <- function(unit, when, instant, id_col, time_col) {
  holes <- c(anyNA(unit), anyNA(when))
  missing <- c(id_col, time_col)[holes]
  if (length(missing)) {
    stop("missing values in ", paste(sprintf("`%s`", missing), collapse = " and "),
         ". Fill or drop them before building a representation.", call. = FALSE)
  }
  # Sort by (unit, time) and look at neighbours. Pasting the two into a key would be the obvious
  # way and builds one string per reading, which on a record of tens of millions of readings costs
  # more memory than the readings themselves. The instants are the whole seconds the calendar is
  # read at, so two readings a fraction of a second apart are the same reading twice here.
  n <- length(unit)
  if (n < 2L) {
    return(invisible(TRUE))
  }
  code <- match(unit, sort(unique(unit), method = "radix"))
  o <- order(code, instant, method = "radix")
  same <- code[o][-1L] == code[o][-n] & instant[o][-1L] == instant[o][-n]
  if (any(same)) {
    first <- o[which(same)[1L] + 1L]
    stop(sum(same), " duplicated (unit, time) pair", if (sum(same) > 1L) "s" else "",
         ", first: ", unit[first], " at ", .iso_instant(when[first]), ".", call. = FALSE)
  }
  invisible(TRUE)
}

# An instant written the one way both languages write one: ISO 8601 in UTC, to the second.
.iso_instant <- function(x) {
  format(x, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
}

# The calendar a series is carried in: the `tzone` of its time column, UTC where it carries none.
# A name the zone database does not know is refused here, because reading a clock in it would
# otherwise fall back to UTC under a warning and bin the record on the wrong calendar.
.series_zone <- function(when, column) {
  if (!inherits(when, "POSIXct")) {
    stop("`", column, "` must be POSIXct, not ", class(when)[1L], ".", call. = FALSE)
  }
  tz <- attr(when, "tzone")
  if (is.null(tz) || !nzchar(tz[1L])) {
    return("UTC")
  }
  tz <- tz[1L]
  if (!tz %in% c("UTC", "GMT", OlsonNames())) {
    stop("`", column, "` is carried in the time zone \"", tz, "\", which the zone database ",
         "does not know. Name one of `OlsonNames()`.", call. = FALSE)
  }
  tz
}

# The zone lives at this boundary and nowhere else. Reading an instant as a clock is defined for
# every instant in every zone; it is the reverse direction, naming a local midnight and asking
# which instant it was, that has no answer on the night a zone skips one.
.naive_seconds <- function(instant, tz, column = "time") {
  if (tz %in% c("UTC", "GMT")) {
    return(instant)
  }
  u <- unique(instant)
  clock <- as.numeric(as.POSIXct(format(.POSIXct(u, tz = tz), "%Y-%m-%d %H:%M:%S"), tz = "UTC"))
  bad <- sum(is.na(clock))
  if (bad) {
    stop("`", column, "` could not be read as a clock in \"", tz, "\" for ",
         bad, " instant", if (bad > 1L) "s" else "", ".", call. = FALSE)
  }
  clock[match(instant, u)]
}

# The instant whose clock in `tz` reads each given local time. The offsets in force a day either
# side bracket any transition, so one of the three candidates is it. A local time the clock skipped
# has no instant at all, and the answer is then the instant the clock jumped to; a local time the
# clock repeated has two, and the answer is the first of them.
.local_to_instant <- function(local, tz) {
  if (!length(local) || tz %in% c("UTC", "GMT")) {
    return(.POSIXct(local, tz = tz))
  }
  probe <- lapply(c(-86400, 0, 86400), function(shift) {
    at <- local + shift
    local - (.naive_seconds(at, tz) - at)
  })
  candidate <- matrix(unlist(probe), ncol = length(probe))
  reads <- matrix(unlist(lapply(probe, function(t) .naive_seconds(t, tz) == local)),
                  ncol = length(probe))
  out <- vapply(seq_along(local), function(i) {
    if (any(reads[i, ])) min(candidate[i, reads[i, ]]) else max(candidate[i, ])
  }, numeric(1))
  .POSIXct(out, tz = tz)
}

# The bin starts as instants. Every grain but `native` is read on the local clock and its starts
# come back on it, so they go back through the zone; the `native` grain is read on the instant
# itself, so its starts already are one.
.bin_instants <- function(bin_start, grain, tz) {
  if (identical(grain, "native")) .POSIXct(bin_start, tz = tz) else .local_to_instant(bin_start, tz)
}

# A supplied calendar returns instants, and the core reads a clock rather than an instant, so its
# bins go through the same boundary as the readings. A missing one is refused here, before that
# boundary: a bin start that is not a number would otherwise reach the core as a bin at the
# beginning of time, holding the reading its real bin then lacks.
.custom_bins <- function(grain, when, tz, unit) {
  out <- grain(when)
  if (!inherits(out, "POSIXct") || length(out) != length(when)) {
    stop("a `grain` function must return one POSIXct bin start per reading.", call. = FALSE)
  }
  missing <- which(is.na(out))
  if (length(missing)) {
    stop("the supplied calendar gives no bin start for ", .plural(length(missing), "reading"),
         ", first: unit ", unit[missing[1L]], " at ", .iso_instant(when[missing[1L]]),
         ". A calendar returns a bin start for every reading it is handed.", call. = FALSE)
  }
  .naive_seconds(floor(as.numeric(out)), tz)
}

.sampling_step <- function(instant) {
  u <- sort(unique(instant))
  if (length(u) < 2L) 0 else min(diff(u))
}
