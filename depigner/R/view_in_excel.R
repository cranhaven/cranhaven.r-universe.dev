#' View in Excel
#'
#' This function open in Excel a data frame, returning it invisibly to
#' be "pipe-able". The operation is done only in interactive session, so
#' that it has no effect on pipe-chains of code executed in batch.
#'
#' This function is useful for intermediate inspection of
#' data, possibly in the middle of piped transformations.
#'
#' This version: avoid unwanted execution (interactive session only),
#' embed the correct extension in the temporary file, add BOM for Excel
#' pleasure, and return the output invisibly, in case it is the last
#' element of the chain.
#'
#' @note this function was originated during the conversation on Twitter
#'   started at
#'   https://twitter.com/brodriguesco/status/1447468259725434886?s=20
#'
#' @param .data a data frame
#'
#' @return the `.data`, unchanged and invisibly
#' @export
#'
#' @examples
#' \dontrun{
#'   four_cyl_cars <- mtcars %>%
#'     view_in_excel() %>%
#'     dplyr::filter(cyl == 4) %>%
#'     view_in_excel()
#'
#'   four_cyl_cars
#' }
view_in_excel <- function(.data) {
  if (interactive()) {
    tmp <- fs::file_temp("excel", ext = "csv")
    readr::write_excel_csv(.data, tmp)
    fs::file_show(tmp)
  }
  invisible(.data)
}
