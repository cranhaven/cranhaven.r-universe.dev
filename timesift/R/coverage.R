#' Which units reach which bins
#'
#' A representation needs every unit in every bin, and [grain_matrix()] refuses a record where one
#' is missing rather than pad it. This is the same binning laid out so the gaps can be read: how
#' many readings each unit has in each bin, over every bin the calendar tiles the record with from
#' the first bin any unit touches to the last. A logger that started late, stopped early or lost a
#' month is a row with zeros in it; a bin the whole record skips is a column of zeros.
#'
#' What to do about a gap is the analyst's decision, and this is the table it is made on: drop the
#' units that do not span the record, cut the record to the span every unit covers, or move to a
#' grain the gap does not reach. Nothing here fills a cell.
#'
#' @inheritParams grain_matrix
#'
#' @return An integer matrix of reading counts, one row per unit and one column per bin, of class
#'   `timesift_coverage`, with the units and the ISO-8601 bin starts as dimnames and the `grain`
#'   and the `bin_start` instants as attributes.
#'
#' @examples
#' t <- seq(as.POSIXct("2021-09-01", tz = "UTC"), by = "hour", length.out = 24 * 40)
#' d <- data.frame(plot = rep(c("a", "b"), each = length(t)), t = rep(t, 2),
#'                 temp = rnorm(2 * length(t)))
#' # Unit b loses the calendar week beginning Monday 6 September.
#' lost <- d$plot == "b" & d$t >= as.POSIXct("2021-09-06", tz = "UTC") &
#'   d$t < as.POSIXct("2021-09-13", tz = "UTC")
#' coverage(d[!lost, ], plot, t, grain = "week")
#'
#' @export
coverage <- function(data, id, time, grain = "day", year_start = "09-01") {
  id_col <- .resolve_column(substitute(id), data, parent.frame())
  time_col <- .resolve_column(substitute(time), data, parent.frame())
  grain <- .check_grain(grain)
  if (length(grain) > 1L) {
    stop("`coverage()` reads one grain at a time.", call. = FALSE)
  }
  ys <- .parse_year_start(year_start)

  unit <- .unit_names(data[[id_col]], id_col)
  when <- data[[time_col]]
  tz <- .series_zone(when, time_col)
  instant <- floor(as.numeric(when))
  .check_readings(unit, when, instant, id_col, time_col)
  local <- .naive_seconds(instant, tz, time_col)

  units <- sort(unique(unit), method = "radix")
  supplied <- if (is.function(grain)) .custom_bins(grain, when, tz, unit) else NULL
  got <- ts_coverage_(match(unit, units), instant, local, supplied, units,
                      if (is.function(grain)) "custom" else grain, ys$month, ys$day)

  bins <- .bin_instants(got$bin_start, grain, tz)
  out <- matrix(got$count, nrow = length(units), ncol = length(bins),
                dimnames = list(units, .iso_instant(bins)))
  attr(out, "grain") <- if (is.function(grain)) "custom" else grain
  attr(out, "bin_start") <- bins
  class(out) <- c("timesift_coverage", class(out))
  out
}

#' @export
print.timesift_coverage <- function(x, ...) {
  d <- dim(x)
  empty <- x == 0L
  cat("<timesift coverage>", .plural(d[1L], "unit"), "x", .plural(d[2L], "bin"),
      "at the", attr(x, "grain"), "grain\n")
  if (!any(empty)) {
    cat("every unit reaches every bin\n")
    return(invisible(x))
  }
  units <- rownames(x)[rowSums(empty) > 0L]
  cat(.plural(sum(empty), "empty (unit, bin) cell"), "in",
      paste0(.plural(length(units), "unit"), ":"), .listing(units), "\n")
  skipped <- colnames(x)[colSums(empty) == d[1L]]
  if (length(skipped)) {
    cat(.plural(length(skipped), "bin"), "no unit reaches:", .listing(skipped), "\n")
  }
  invisible(x)
}
