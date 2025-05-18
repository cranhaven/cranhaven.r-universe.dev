#' Get desired STICS outputs
#'
#' @description Get the STICS output variables (from var.mod file)
#'
#' @param workspace Path of the directory containing the STICS var.mod file
#' @param file_name file name to read (without path, default value: "var.mod")
#'
#' @return The variables that will be returned by STICS
#' @export
#'
#' @seealso `gen_varmod`
#' @examples
#' get_varmod(get_examples_path(file_type = "txt"))
get_varmod <- function(workspace,
                       file_name = "var.mod") {
  file_path <- file.path(workspace, file_name)

  if (!file.exists(file_path)) {
    stop(paste(file_path, ": does not exist !"))
  }

  readLines(file_path)
}
