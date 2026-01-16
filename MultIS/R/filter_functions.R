#' Converts a matrix to relative abundances
#'
#' @param data A matrix of readouts that should be converted to relative
#'             abundances
#'
#' @export
#' @return The matrix with all columns in percent
convert_columnwise_relative <- function(data) {
  stopifnot(is.matrix(data))
  stopifnot(nrow(data) > 0)

  clss <- class(data)
  cols <- colSums(data) != 0
  if (length(cols) == 0) {
    return(data)
  }
  data[, cols] <- t(t(data[, cols, drop = FALSE]) /
                     colSums(data[, cols, drop = FALSE], na.rm = TRUE))
  class(data) <- clss
  return(data)
}

#' Filters a matrix of readouts for IS that have a minimum occurrence in some
#' measurement
#'
#' @param data The readout matrix to filter.
#' @param at A filter for the columns/measurements. Only matching
#'           columns/measurements are considered, though all will be returned.
#'           This is a regular expression, so multiple columns/measurements may
#'           match it.
#' @param min The minimum with which an IS has to occur. This could be either
#'            absolute or relative reads. If `at` matches multiple
#'            columns/measurements, the 'rowSum()' over the columns will be
#'            used.
#'
#' @return A matrix with only the IS that occur with a minimum at the
#'         selected measurements.
#' @export
filter_at_tp_min <- function(data, at = "168", min = 0.02) {
  stopifnot(length(min) == 1)
  if (!is.numeric(min)) {
    warning("Argument min is not numeric, converting now")
    min <- as.numeric(min)
  }
  clss <- class(data)
  # given time point
  tp <- data[, grep(paste0("^.*", at, ".*$"), colnames(data)), drop = FALSE]
  # at least min at the last time point
  tp_gt_min <- rowSums(tp >= min, na.rm = TRUE) > 0
  data <- data[tp_gt_min, , drop = FALSE]
  class(data) <- clss
  return(data) # the data for the previous filters
}

#' Filters a matrix of readouts for the n biggest IS at a certain
#' measurement
#'
#' @param data The readout matrix to filter.
#' @param at A filter for the columns/measurement. Only matching
#'           columns/measurements are considered, though all will be returned.
#' @param n The number of biggest IS to return. If `at` matches multiple
#'          columns/measurements, the 'rowSum()' over the columns/measurements
#'          will be used. For ties, more than `n` IS may be returned.
#'
#' @return A matrix with only the n biggest IS at the selected measurements.
#' @export
filter_at_tp_biggest_n <- function(data, at = "168", n = 50) {
  stopifnot(length(n) == 1)
  if (!is.integer(n)) {
    warning("Argument n is not an integer, converting now")
    n <- as.integer(n)
  }
  clss <- class(data)
  # given time point
  tp <- data[, grep(paste0("^.*", at, ".*$"), colnames(data)), drop = FALSE]
  if (ncol(tp) > 1) {
    tp <- rowSums(tp)
    data <- data[rank(tp) > length(tp) - n, ]
  } else {
    data <- data[rank(tp) > nrow(tp) - n, ]
  }

  class(data) <- clss
  return(data)
}

#' Filters for a minimum number of time points/measurements
#'
#' @export
#' @param dat The readout matrix to filter.
#' @param min The minimum number of measurements where an IS needs to have a
#'            value that is not 0 or NA.
#' @return A matrix with only ISs that have more than `min` columns that are
#'         not 0 or NA.
filter_nr_tp_min <- function(dat, min = 6) {
  stopifnot(length(min) == 1)
  if (!is.integer(min)) {
    warning("min is not an integer, converting now")
    min <- as.integer(min)
  }
  clss <- class(dat)
  dat <- dat[rowSums(!is.na(dat), na.rm = TRUE) >= min, , drop = FALSE]
  dat <- dat[rowSums(dat != 0, na.rm = TRUE) >= min, , drop = FALSE]
  class(dat) <- clss
  return(dat)
}

#' Filters a vector of names and returns the shortest common prefix.
#'
#' @export
#' @param names The vector of names to filter.
#' @param by A regexp that splits the string. The default filters by special
#'           characters. A split by character can be achieved by using "." as
#'           the regexp.
#' @return The names shortened to the shortest prefix (in chunks defined by
#'         the regexp) where all names are unique.
filter_names <- function(names, by = "[_():]|[^_():]*") {
  names <- regmatches(names, gregexpr(by, names))

  max_len <- max(unlist(lapply(names, length)))
  for (shrtst in 1:max_len) {
    # might pad names that are too short with NA
    tmp <- lapply(names, `[`, 1:shrtst)
    if (!anyDuplicated(tmp)) {
      break
    }
  }

  return(lapply(tmp, function(x) {
    paste(stats::na.omit(x), collapse = "")
  }))
}

#' Shortens the rownames of a readout matrix to the shortest distinct prefix
#'
#' @export
#' @param dat The readout matrix for which the names should be filtered.
#' @param by The regexp used to split the names.
#' @return A matrix with the names filtered to the shortest unique prefix.
#' @seealso filter.names
filter_is_names <- function(dat, by = "[_():]|[^_():]*") {
  rownames(dat) <- filter_names(rownames(dat), by = by)
  return(dat)
}

#' Filters for columns containing a certain substring.
#'
#' @export
#' @param dat The readout matrix to filter.
#' @param match The substring that columns must match.
#' @return A readout matrix that only contains the columns whose names contain
#'         the substring.
filter_match <- function(dat, match = "E2P11") {
  clss <- class(dat)
  dat <- dat[, grep(paste0(".*", match, ".*"), colnames(dat))]
  class(dat) <- clss
  return(dat)
}

#' Splits a vector of strings by a given regexp, selects and rearranges the
#' parts and joins them again
#'
#' @export
#' @param dat The readout matrix to filter.
#' @param elems The elements to select. They are rearrange in the order that is
#'              given via this argument.
#' @param by The string used for splitting the names of the columns.
#' @return A matrix where the names of the columns are split by the given
#'         string, rearranged and again joined by the string.
filter_measurement_names <- function(dat, elems = c(1, 3), by = "_") {
  clss <- class(dat)
  old_cn <- colnames(dat)
  new_cn <- unlist(lapply(old_cn, function(x) {
    paste(strsplit(x, split = by)[[1]][elems], collapse = by)
  }))
  colnames(dat) <- new_cn
  class(dat) <- clss
  return(dat)
}

#' Combines columns that have the same name. The columns are joined additively.
#'
#' @export
#' @param dat The readout matrix to filter.
#' @param pre_norm Whether to normalize columns before joining them.
#' @param post_norm Whether to normalize columns after they are joined.
#' @return A matrix in which columns that had the same name are added and
#'         (possibly) normalized.
filter_combine_measurements <- function(dat,
                                        pre_norm = TRUE,
                                        post_norm = TRUE) {
  clss <- class(dat)

  nm <- colnames(dat)
  ret <- matrix(data = NA,
                nrow = nrow(dat),
                ncol = length(unique(nm)),
                dimnames = list(rownames(dat), unique(nm)))

  for (ucn in unique(nm)) {
    cols <- nm == ucn
    ss <- dat[, cols, drop = FALSE]
    if (pre_norm) {
      ss <- t(t(ss) / colSums(ss, na.rm = TRUE))
    }
    ret[, ucn] <- rowSums(ss, na.rm = TRUE)
  }

  if (post_norm) {
    ret <- t(t(ret) / colSums(ret, na.rm = TRUE))
  }

  class(ret) <- clss
  return(ret)
}

#' Removes rows that only contain 0 or NA.
#'
#' @export
#' @param dat The readout matrix to filter.
#' @return A matrix where rows that where only 0 or NA are filtered out.
filter_zero_rows <- function(dat) {
  clss <- class(dat)
  dat <- dat[rowSums(dat, na.rm = TRUE) != 0, , drop = FALSE]
  class(dat) <- clss
  return(dat)
}

#' Removes columns that only contain 0 or NA.
#'
#' @export
#' @param dat The readout matrix to filter.
#' @return A matrix where columns that where only 0 or NA are filtered out.
filter_zero_columns <- function(dat) {
  clss <- class(dat)
  dat <- dat[, colSums(dat, na.rm = TRUE) != 0, drop = FALSE]
  class(dat) <- clss
  return(dat)
}
