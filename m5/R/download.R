#' Download and unzip the raw data to the specified directory
#'
#' @param path A directory name to save the zip file
#' @param unzip Automatically uznip the file when the downloading is finished.
#' Default: TRUE. The `exdir` argument in the `unzip` function is the directory
#' name the file was downloaded into.
#'
#' @returns
#' Returns nothing; the result of the function call is the `m5.zip` file
#' downloading and extraction.
#'
#' @note
#' If you struggle with timeout problems, please increase the timeout value
#' using `options(timeout=<new_timeout_value>)`
#'
#' @examples
#' \donttest{
#' m5_download('data')
#' }
#'
#' @importFrom utils download.file
#'
#' @references
#' * [M5 Forecasting - Accuracy](https://www.kaggle.com/c/m5-forecasting-accuracy)
#' * [M5 Forecasting - Uncertainty](https://www.kaggle.com/c/m5-forecasting-uncertainty)
#'
#' @export
m5_download <- function(path, unzip=TRUE){
  url <- "https://github.com/krzjoa/m5-data/raw/main/datasets/m5.zip"
  if (!file.exists(path))
    dir.create(path)
  path <- file.path(path, "m5.zip")
  download.file(url, path)
  if (unzip)
    utils::unzip(path, exdir = dirname(path))
}
