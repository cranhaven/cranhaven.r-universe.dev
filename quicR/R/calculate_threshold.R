#' Calculate a Threshold for Rate Determination
#'
#' Calculates a threshold for determining time-to-threshold and rate of amyloid formation.
#'
#' @param data A dataframe output from get_real.
#' @param background_cycle Integer; the cycle used for background fluorescence.
#' @param method Method for determining threshold; default is "stdev".
#' @param multiplier For some methods, will add a multiplier for more conservative thresholds.
#'
#' @importFrom dplyr select
#' @importFrom stats sd
#'
#' @return A float value.
#'
#' @examples
#' file <- system.file(
#'   "extdata/input_files",
#'   file = "test2.xlsx",
#'   package = "quicR"
#' )
#' threshold <- get_real(file)[[1]] |>
#'   calculate_threshold(multiplier = 10)
#'
#' @export
calculate_threshold <- function(data, background_cycle = 2, method = list("stdev", "none"), multiplier = 1) {

  if (is.list(method)) method <- method[[which(method == "stdev")]]
  if (method == "none") return(NA)

  curate <- function() {
    data |>
      select(-"Time") |>
      lapply(as.numeric) |>
      as.data.frame()
  }

  if (method == "stdev") {
    get_row <- function() curate()[background_cycle, ]
    avg <- function() apply(get_row(), 1, mean, na.rm = TRUE)[[1]]
    std <- function() apply(get_row(), 1, sd, na.rm = TRUE)[[1]] * multiplier
    return(avg() + std())
  }

}
