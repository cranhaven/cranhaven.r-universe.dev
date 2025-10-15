#' Calculate the Maxpoint Ratio
#'
#' Maxpoint ratio is defined as the maximum relative fluorescence divided by the
#' background fluorescence.
#'
#' @param data A dataframe containing the real-time fluorescence data.
#' @param start_col Integer, the column at which the background fluorescence should be read.
#' @param data_is_norm Logical, if the data has not been normalized, will make a call to normalize_RFU.
#' @return A vector containing MPR values.
#'
#' @importFrom dplyr select
#' @importFrom dplyr %>%
#'
#' @examples
#' # This test takes >5 sec
#' \donttest{
#' file <- system.file(
#'   "extdata/input_files",
#'   file = "test.xlsx",
#'   package = "quicR"
#' )
#' df_ <- quicR::get_real(file)[[1]]
#' print(calculate_MPR(df_))
#' }
#'
#' @export
calculate_MPR <- function(data, start_col = 3, data_is_norm = TRUE) {
  data %>%
    {
      if (data_is_norm) . else quicR::normalize_RFU(.)
    } %>%
    select(start_col:ncol(.)) %>%
    apply(1, max) %>%
    return()
}
