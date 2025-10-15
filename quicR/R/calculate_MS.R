#' Calculate Maximum Slope
#'
#' Uses a sliding window to calculate the slope of real-time reads.
#'
#' @param data A dataframe containing real-time reads. It is recommended to use a dataframe made from normalize_RFU.
#' @param window Integer designating how wide you want the sliding window to be for calculating the moving average slope.
#' @param data_is_norm Logical; if FALSE, will make a call to normalize_RFU.
#'
#' @return A dataframe containing the real-time slope values as change in RFU/sec.
#'
#' @importFrom slider slide_dbl
#' @importFrom purrr map_dbl
#' @importFrom dplyr mutate_all
#' @importFrom dplyr mutate
#' @importFrom dplyr relocate
#' @importFrom dplyr %>%
#' @importFrom dplyr select
#'
#' @examples
#' # This test takes >5 sec
#' \donttest{
#' file <- system.file(
#'   "extdata/input_files",
#'   file = "rt_data.csv",
#'   package = "quicR"
#' )
#' df_ <- read.csv(file, check.names = FALSE)
#' calculate_MS(df_)
#' }
#'
#' @export
calculate_MS <- function(data, window = 3, data_is_norm = TRUE) {
  curate <- function(x) {
    x %>%
      {if (data_is_norm) . else normalize_RFU(.)} %>%
      t() %>%
      as.data.frame() %>%
      mutate_all(~ as.numeric(as.character(.))) %>%
      mutate(Time = as.numeric(rownames(.))) %>%
      suppressWarnings() %>%
      na.omit() %>%
      relocate("Time", .before = 1)
  }

  slope <- function(x) {
    x %>%
      select(-"Time") %>%
      map_dbl(~ {
        slide_dbl(
          .x = seq_along(.x),
          .f = function(idx) {
            if (length(idx) < 2) return(NA)
            diff(range(.x[idx])) / diff(range(x[["Time"]][idx]))
          },
          .before = window,
          .complete = TRUE
        ) %>%
          max(na.rm = TRUE)
      })
  }

  return(slope(curate(data)))
}
