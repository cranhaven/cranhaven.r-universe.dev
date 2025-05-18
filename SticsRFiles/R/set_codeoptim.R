#' @title Set codoptim
#' @description Change value of codeoptim in the new_travail.usm file
#' @param workspace Path of the STICS inputs files of an usm
#' @param value Value of the codeoptim parameter (1 = activating
#' parameters values forcing)
#' @param file_name Name of the file
#'
#' @examples
#' \dontrun{
#'
#' ws <- "path/to/stics/workspace"
#' set_codoptim(workspace = ws, value = 0)
#' }
#'
#' @keywords internal
#'
#' @noRd
#'
set_codeoptim <- function(workspace, value = 1, file_name = "new_travail.usm") {
  if (!dir.exists(workspace)) {
    stop(paste(workspace, "directory does not exist !"))
  }

  file_path <- file.path(workspace, file_name)

  if (!file.exists(file_path)) {
    stop(paste(file_path, "does not exist !"))
  }

  lines <- readLines(file_path)

  idx1 <- grep("codeoptim", lines) + 1
  idx2 <- grep("codoptim", lines) + 1 # for compat with STICS V10 ...

  if (length(idx1) > 0) {
    idx <- idx1
  } else if (length(idx2) > 0) {
    idx <- idx2
  } else {
    stop(paste("Neither codeoptim nor codoptim found in", file_path))
  }

  lines[idx] <- as.character(value)

  writeLines(lines, file_path)
}
