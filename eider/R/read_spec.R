#' Read spec type and give appropriate logging messages
#'
#' @param filename_or_json_str A string which may or may not be a valid filepath
#'
#' @return information if a file or a string has been provided
#' @noRd
read_spec_type <- function(filename_or_json_str, context = NULL) {
  context <- c(context, "read_spec_type")
  trace_context(context)

  result <- chk_pth(filename_or_json_str)
  if (result == "valid") {
    file_or_string <- "file"
    debug_context(
      context = context,
      message = paste0("Valid filepath found: ", filename_or_json_str)
    )
  } else if (result == "too_long") {
    file_or_string <- "string"
    debug_context(
      context = context,
      message = "Specified path is too long for a file, assuming a json string"
    )
  } else if (result == "not_valid") {
    file_or_string <- "string"
    debug_context(
      context = context,
      message = "No valid filepath found, assuming a json string"
    )
  } else if (result == "other_error") {
    stop_context("Unknown error when trying to read specification", context)
  }

  return(file_or_string)
}

#' Check to see if provided spec is path or a JSON string.
#'
#' @param file_or_json A string corresponding to either a filepath containing
#' the specification, or the specification itself as a json string
#'
#' @return A string identifying if the file was found or not
#' @noRd
chk_pth <- function(file_or_json) {
  tryCatch(
    expr = {
      is_valid_file <- fs::is_file(file_or_json)
      if (is_valid_file) {
        return("valid")
      } else {
        return("not_valid")
      }
    },
    error = function(e) {
      too_long <- stringr::str_detect(e[[1]], "NAMETOOLONG")
      if (too_long) {
        return("too_long")
      } else {
        return("other_error")
      }
    }
  )
}
