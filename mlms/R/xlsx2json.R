#' Convert Field Data from XLSX to JSON
#'
#' @description Convert the file format of field data from XLSX to JSON.
#'
#' @param path 'character' string.
#'   Either the path to the Excel workbook file (XLSX) to read,
#'   or the directory containing XLSX files.
#' @param destdir 'character' string.
#'   Destination directory to write JSON file(s).
#'   Defaults to the `path` directory.
#'
#' @return File path(s) of the JSON formatted data.
#'
#' @author J.C. Fisher, U.S. Geological Survey, Idaho Water Science Center
#'
#' @seealso [`read_field_xlsx`] function for reading field data from a XLSX file.
#' @seealso [`write_field_json`] function for writing field data to a JSON file.
#'
#' @export
#'
#' @examples
#' path <- system.file("extdata/ex-field.xlsx", package = "mlms") |>
#'   xlsx2json(destdir = tempdir())
#'
#' unlink(path)

xlsx2json <- function(path, destdir) {

  # check arguments
  checkmate::assert_string(path)
  checkmate::assert_string(destdir, null.ok = TRUE)

  # set XLSX file path(s)
  path <- path.expand(path) |> normalizePath(winslash = "/", mustWork = FALSE)
  if (utils::file_test("-f", path)) {
    checkmate::assert_file_exists(path, access = "r", extension = "xlsx")
  } else {
    checkmate::assert_directory_exists(path, access = "rw")
    path <- list.files(
      path = path,
      pattern = "\\.xlsx$",
      full.names = TRUE,
      recursive = TRUE
    )
    stopifnot("No XLSX files found under directory path." = length(path) > 0)
  }

  # set destination directory
  if (missing(destdir)) {
    destdir <- dirname(path[1])
  } else {
    destdir <- path.expand(destdir) |> normalizePath(winslash = "/", mustWork = FALSE)
    dir.create(path = destdir, showWarnings = FALSE)
  }
  checkmate::assert_directory_exists(destdir, access = "rw")

  # create temporary directory
  tempdir <- tempfile("")
  dir.create(path = tempdir)
  on.exit({
    unlink(tempdir, recursive = TRUE)
  })

  # set file paths
  json_names <- basename(path) |> tools::file_path_sans_ext() |> paste0(".json")
  temp_paths <- file.path(tempdir, json_names, fsep = "/")
  json_paths <- file.path(destdir, json_names, fsep = "/")

  # read XLSX files and write to JSON files in temporary directory
  for (i in seq_along(path)) {
    read_field_xlsx(path = path[i]) |>
      write_field_json(path = temp_paths[i])
  }

  # copy JSON files from temporary to destination directory
  file.copy(from = temp_paths, to = json_paths, overwrite = TRUE)

  invisible(json_paths)
}
