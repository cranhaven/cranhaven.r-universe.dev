#' Separating overlapping prescription periods
#'
#' Run this function and then you can either simply discard overlapping
#' intervals or shift them around using an appropriate algorithm.
#'
#' The older implementation used \code{isolateoverlaps} from the
#' \code{intervalaverage} package and \code{Overlap} from the \code{DescTools}
#' package. Here we refactor it using functions from \code{tidyverse} instead.
#'
#' @param data A data frame including variables \code{patid}, \code{start_date},
#' \code{stop_date} and \code{prodcode}
#'
#' @return A data frame of \code{patid}, \code{prodcode}, \code{start_date} and
#' \code{stop_date}, where intervals are either exactly overlapping or mutually
#' non-overlapping (but not partially overlapping), such that the union of such
#' intervals is equivalent to those originally provided in \code{data}
#'
#' @examples
#' set.seed(1)
#' overlapping_data <- data.frame(
#'   rowid = 1:20,
#'   patid = 1:2,
#'   prodcode = 'a',
#'   start_date = Sys.Date() + c(round(rexp(19, 1/7)), -20),
#'   qty = rpois(20, 64),
#'   ndd = sample(seq(.5, 12, by = .5), 20, replace = TRUE),
#'   stringsAsFactors = FALSE
#' )
#' overlapping_data <- transform(overlapping_data,
#'   stop_date = start_date + qty / ndd
#' )
#' isolate_overlaps(overlapping_data)
#'
#' @note
#' This function currently doesn't use any keys except \code{patid} and
#' \code{prodcode}. It may be desirable to add a row ID, for matching each
#' partial interval back to the original interval from which it was derived.
#' This may be relevant to models using weighted dosages.
#'
#' @seealso
#' \code{intervalaverage::isolateoverlaps},
#' \code{\link[data.table]{foverlaps}}
#'
#' @import dplyr tidyr
#' @importFrom sqldf sqldf
#' @importFrom rlang .data
#' @export
isolate_overlaps <- function(data) {
  data %>%
    # melt
    tidyr::pivot_longer(c(.data$start_date, .data$stop_date),
                        names_to = 'date_name',
                        values_to = 'date_value') %>%
    # setorderv
    dplyr::arrange(.data$patid, .data$prodcode,
                   .data$date_value, .data$date_name) %>%
    dplyr::group_by(.data$patid, .data$prodcode) %>%
    # shift
    dplyr::transmute(.data$date_value,
                     is_end_var = .data$date_name == 'stop_date',
                     end_next_var = dplyr::lead(.data$is_end_var),
                     value_next_var = dplyr::lead(.data$date_value)) %>%
    dplyr::filter(!is.na(.data$end_next_var)) %>%
    # temp
    dplyr::transmute(start_date = dplyr::if_else(!.data$is_end_var,
                                                 .data$date_value,
                                                 .data$date_value + 1L),
                     stop_date = dplyr::if_else(!.data$end_next_var,
                                                .data$value_next_var - 1L,
                                                .data$value_next_var)) %>%
    dplyr::filter(.data$stop_date >= .data$start_date) -> temp

  # foverlaps / overlap join
  out <- sqldf::sqldf(
    'SELECT x.patid, x.prodcode, --x.rowid, y.rowid yrowid,
            y.start_date, y.stop_date
     FROM data x
     JOIN temp y
     ON (x.start_date BETWEEN y.start_date AND y.stop_date) OR
        (x.stop_date BETWEEN y.start_date AND y.stop_date) OR
        (x.start_date < y.start_date AND x.stop_date > y.stop_date) OR
        (x.start_date > y.start_date AND x.stop_date < y.stop_date)
     WHERE x.patid = y.patid AND x.prodcode = y.prodcode'
  )

  return(out)
}

#' Close small gaps between successive prescriptions
#'
#' Given a series of prescriptions in \code{data}, if one prescription
#' (for the same patient and drug) starts \eqn{\leq} \code{min_gap} days
#' after the previous one finishes, we extend the length of the previous
#' prescription to cover the gap.
#'
#' @param data A data frame containing columns \code{prodcode}, \code{patid},
#' \code{start_date} and \code{stop_date}
#' @param min_gap Size of largest gaps to close. Default is zero, i.e. do nothing
#'
#' @return The input data frame \code{data}, possibly with some of the
#' \code{stop_date}s changed.
#'
#' @examples
#' gappy_data <- data.frame(
#'   patid = 1,
#'   prodcode = 'a',
#'   start_date = Sys.Date() + (0:6) * 7,
#'   stop_date = Sys.Date() + (0:6) * 7 + 4
#' )
#' close_small_gaps(gappy_data)
#' close_small_gaps(gappy_data, 7)
#'
#' @import dplyr
#' @importFrom rlang .data
#' @export
close_small_gaps <- function(data, min_gap = 0L) {
  if ((min_gap <- round(min_gap)) < 0)
    stop('min_gap must non-negative')
  if (length(min_gap) != 1)
    stop('min_gap must be a single value')
  if ('next_start_date' %in% colnames(data))
    warning('`next_start_date` is a reserved variable name but already exists')

  data %>%
    dplyr::group_by(.data$patid, .data$prodcode) %>%
    dplyr::mutate(
      next_start_date = dplyr::lead(.data$start_date, order_by = .data$start_date),
      gap = difftime(.data$next_start_date, .data$stop_date, units = 'days'),
      stop_date = dplyr::if_else(.data$gap < min_gap & .data$gap >= 0,
                                 .data$next_start_date,
                                 .data$stop_date,
                                 missing = .data$stop_date)
    ) %>%
    dplyr::select(-.data$next_start_date, -.data$gap) %>%
    dplyr::ungroup()
}

#' Shift time intervals until they no longer overlap
#'
#' This is a function used by \code{\link{decision_9}}.
#'
#' @param x a data frame containing variables \code{start_date},
#' \code{stop_date} and \code{patid}
#'
#' @return
#' A data frame with time intervals moved such that they no longer overlap
#'
#' @import dplyr
#' @importFrom rlang .data
#' @importFrom DescTools Overlap
#' @export
shift_interval <- function(x) {
  x$id <- seq_along(x$patid)
  i <- 0
  y <- x
  overlap <- TRUE
  while (overlap) {
    i <- i + 1
    overlaps <- y %>%
      dplyr::mutate(dummy = 1) %>%
      dplyr::full_join(., ., by = "dummy") %>%
      dplyr::filter(.data$id.x < .data$id.y) %>%
      dplyr::mutate(overlap = DescTools::Overlap(
        cbind(.data$start_date.x, .data$stop_date.x),
        cbind(.data$start_date.y, .data$stop_date.y)
      )) %>%
      dplyr::filter(.data$overlap > 0)
    y <- overlaps %>%
      dplyr::select(id = .data$id.y, .data$overlap) %>%
      dplyr::slice(1) %>%
      dplyr::right_join(y, by = "id") %>%
      dplyr::mutate(
        overlap = replace(.data$overlap, is.na(.data$overlap), 0),
        start_date = .data$start_date + .data$overlap,
        stop_date = .data$stop_date + .data$overlap
      ) %>%
      dplyr::select(-.data$overlap)
    overlap <- nrow(overlaps)
  }
  return(y)
}
