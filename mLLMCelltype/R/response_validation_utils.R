is_error_response <- function(result) {
  is.character(result) && any(grepl("^\\s*error\\s*:", result, ignore.case = TRUE))
}
