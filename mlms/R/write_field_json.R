#' Write Field Data in JSON format
#'
#' @description Write field visit monitoring event data to JSON format.
#'
#' @param x 'list' with compoents of class 'mlms.sheet'.
#'   See returned object from the [`read_field_xlsx`] function.
#' @param path 'character' string.
#'   Path to the JSON file.
#' @param overwrite 'logical' flag.
#'   Whether to overwrite the JSON file.
#'
#' @return Worksheet data as a 'list' with compoents of class 'mlms.sheet'.
#'
#' @author J.C. Fisher, U.S. Geological Survey, Idaho Water Science Center
#'
#' @seealso [`read_field_json`] function for reading field data in JSON format.
#'
#' @export
#'
#' @keywords internal
#'
#' @examples
#' sheets <- read_field_xlsx(
#'   path = system.file("/extdata/ex-field.xlsx", package = "mlms")
#' )
#'
#' path <- tempfile("sheets-", fileext = ".json")
#' write_field_json(sheets, path = path)
#' if (interactive()) file.show(path)
#'
#' unlink(path)

write_field_json <- function(x, path, overwrite = FALSE) {

  # check arguments
  checkmate::assert_list(x)
  checkmate::assert_path_for_output(path,
    extension = "json",
    overwrite = overwrite
  )
  checkmate::assert_flag(overwrite)

  # print status
  message("Writing JSON file:\n  ", path)

  # write json file
  jsonlite::write_json(x,
    path = path,
    pretty = TRUE,
    null = "null",
    na = "null",
    auto_unbox = TRUE
  )
}
