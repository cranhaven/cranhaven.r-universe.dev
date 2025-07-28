#' Create HTML Download Button
#'
#' @description Create a download button that exports the selected table data
#'   to a comma-separated values (CSV) file when clicked.
#'
#' @param table_id 'character' string.
#'   Element identifier for the table widget.
#' @param ver 'package_version' or 'character' string.
#'   Version identifier inserted into the CSV file name.
#'
#' @return Returns an object of class 'shiny.tag'.
#'
#' @author J.C. Fisher, U.S. Geological Survey, Idaho Water Science Center
#'
#' @export
#'
#' @keywords internal
#'
#' @examples
#' html <- make_dl_button("test")

make_dl_button <- function(table_id, ver = utils::packageVersion("mlms")) {

  # check packages
  if (!requireNamespace("fontawesome", quietly = TRUE)) {
    stop("Creating a download button requires the 'fontawesome' package.", call. = FALSE)
  }
  if (!requireNamespace("htmltools", quietly = TRUE)) {
    stop("Creating a download button requires the 'htmltools' package.", call. = FALSE)
  }

  # check arguments
  checkmate::assert_string(table_id)
  checkmate::assert_string(format(ver), null.ok = TRUE)

  # make file name
  if (is.null(ver)) {
    file_nm <- sprintf("%s.csv", table_id)
  } else {
    file_nm <- sprintf("%s-%s.csv", table_id, ver)
  }

  # define button attributes
  icon <- fontawesome::fa("download")
  onclick <- sprintf("Reactable.downloadDataCSV('%s', '%s')", table_id, file_nm)
  title <- "Download the filtered data table as a Comma-Separated Values (CSV) formatted file."

  # make button
  htmltools::tagList(icon, "Download as CSV") |>
    htmltools::tags$button(onclick = onclick, title = title) |>
    htmltools::tagList() |>
    htmltools::p()
}
