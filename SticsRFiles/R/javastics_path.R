#' Getting JavaSTICS path from javastics_path environment variable
#'
#' @return Path of the JavaSTICS root directory if javastics_path
#' environment variable exists
#'
#' @keywords internal
#'
#' @noRd
#'
# @examples
get_javastics_path <- function() {

  attr(exists_javastics_path(), "path")

}


#' Set JavaSTICS path in `javastics_path` environment variable
#'
#' @param javastics_path Absolute path of the root directory of JavaSTICS
#' @param write boolean, TRUE if `javastics_path` has to be written in
#' a `.Renviron` file, FALSE otherwise
#'
# @return
#' @keywords internal
#'
#' @noRd
#'
# @examples
set_javastics_path <- function(javastics_path, write = FALSE) {

  if (!dir.exists(javastics_path)) {
    stop(javastics_path, ": is not an existing path, aborting !")
  }

  Sys.setenv(javastics_path = javastics_path)

  if (write) write_javastics_path(javastics_path)

}


#' Evaluates if a `javastics_path` environment variable exists
#'
#' @return boolean, TRUE if the variable exists, FALSE otherwise.
#' A `path` attribute is added to the object `path` containing
#' the variable content.
#'
#' @keywords internal
#'
#' @noRd
#'
# @examples
exists_javastics_path <- function() {

  path <- Sys.getenv("javastics_path")

  path_exists <- TRUE

  if (path == "") path_exists <- FALSE

  attr(path_exists, "path") <- path

  return(path_exists)

}


#' Write the `javastics_path` variable in a `.Renviron` file, or replace its
#' content if it already exists.
#'
#' @param javastics_path Absolute path of the root directory of JavaSTICS
#'
# @return
#' @keywords internal
#'
#' @noRd
#'
# @examples
write_javastics_path <- function(javastics_path) {

  renviron_path <- file.path(path.expand("~"), ".Renviron")
  env_string <- paste0('javastics_path="', javastics_path, '"')

  if (file.exists(renviron_path)) {
      content <- readLines(renviron_path)
      idx <- grep(pattern = "^javastics_path",
                  x = content)
      content[idx] <- env_string
  } else {
    content <- env_string
  }

  writeLines(text = content, con = renviron_path)
}
