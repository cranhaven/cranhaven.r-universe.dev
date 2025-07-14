#' Find the peak date of an incidence curve
#'
# -------------------------------------------------------------------------
#' This function can be used to find the peak of an epidemic curve stored as an
#' `[incidence2]` object.
#'
# -------------------------------------------------------------------------
#' @inheritParams incidence2::keep_peaks
#'
# -------------------------------------------------------------------------
#' @return
#'
#' An `[incidence2]` object the date of the (first) highest incidence in the
#' data along with the count. If `x` is grouped object then the output will have
#' the peak calculated for each grouping.
#'
# -------------------------------------------------------------------------
#' @seealso
#'
#' `estimate_peak()` for bootstrap estimates of the peak time.
#'
# -------------------------------------------------------------------------
#' @examples
#' if (requireNamespace("outbreaks", quietly = TRUE)) {
#'   # load data and create incidence
#'   data(fluH7N9_china_2013, package = "outbreaks")
#'   i <- incidence(fluH7N9_china_2013, date_index = "date_of_onset")
#'   find_peak(i)
#' }
#'
# -------------------------------------------------------------------------
#' @export
find_peak <- function(x, complete_dates = TRUE, ...) {
    keep_peaks(x = x, complete_dates = complete_dates, first_only = TRUE, ...)
}
