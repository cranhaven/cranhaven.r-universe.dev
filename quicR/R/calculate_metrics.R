#' Generate a dataframe with calculated metrics.
#'
#' Uses functions from the "calculate" family of quicR functions to generate an analyzed dataframe.
#'
#' @param data A dataframe containing the raw RT-QuIC data.
#' @param meta A dataframe containing sample metadata. Should include at least the "Sample IDs" column.
#' @param metrics An array containing the metrics which should be calculated.
#' @param transpose Logical; should the raw data be transposed before performing the calculations?
#' @param normalize Logical; should the raw data be normalized before performing the calculations?
#' @param start_col Integer; column number denoting where the numeric data begins.
#' @param MS_window Integer; width of the window applied in the calculation of max slope.
#' @param threshold Float; the threshold applied to the calculation of time-to-threshold.
#'
#' @importFrom dplyr mutate
#'
#' @return A dataframe of calculated metrics.
#'
#' @examples
#' file <- system.file(
#'   "extdata/input_files",
#'   file = "test4.xlsx",
#'   package = "quicR"
#' )
#'
#' data <- quicR::get_real(file)[[1]] |>
#'   quicR::normalize_RFU()
#'
#' meta <- quicR::organize_tables(file) |>
#'   quicR::convert_tables()
#'
#' calculate_metrics(data, meta)
#'
#'@export
calculate_metrics <- function(
    data,
    meta,
    metrics = c("MPR", "MS", "TtT", "RAF"),
    transpose = FALSE,
    normalize = FALSE,
    start_col = 3L,
    MS_window = 3L,
    threshold = 2
) {

  if (transpose) data <- transpose_real(data)
  if (normalize) data <- normalize_RFU(data)
  if ("RAF" %in% metrics & !("TtT" %in% metrics)) {
    stop("RAF can only be calculated using TtT.\nPlease include TtT in the metrics argument. ")
  }

  data.frame(
    `Sample IDs` = if ("Sample IDs" %in% colnames(meta)) meta$`Sample IDs`,
    Dilutions = if ("Dilutions" %in% colnames(meta)) meta$Dilutions,
    check.names = FALSE
  ) |>
    mutate(
      MPR = if ("MPR" %in% metrics) calculate_MPR(data, start_col),
      MS = if ("MS" %in% metrics) calculate_MS(data, MS_window),
      TtT = if ("TtT" %in% metrics) calculate_TtT(data, threshold),
      RAF = if ("RAF" %in% metrics) 1 / .data$TtT
    )
}
