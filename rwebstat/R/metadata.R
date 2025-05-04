#' Get metadata from a dataframe returned by w_data function
#'
#' @param data dataframe from w_data()
#'
#' @return dataframe
#'
#' @export
#'
#' @examples
#' \dontrun{
#' ## Request the US Dollar monthly exchange rates in Euro
#' df <- w_data(dataset_name = "EXR", series_name = "M.USD.EUR.SP00.E")
#' meta <- w_meta(df)
#' }
#'
w_meta <- function(data) {
  attr(data, "metadata")
}
