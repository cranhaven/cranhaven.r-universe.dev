checkInternet <- function(dbase_file) {
  try_GET <- function(x, ...) {
    tryCatch(
      GET(url = x, timeout(10), ...),
      error = function(e) conditionMessage(e),
      warning = function(w) conditionMessage(w)
    )
  }
  is_response <- function(x) {
    class(x) == "response"
  }

  # First check internet connection
  if (!curl::has_internet()) {
    message("No internet connection.")
    return(invisible(NULL))
  }
  # second check for timeout problems
  resp <- try_GET(dbase_file)
  if (!is_response(resp)) {
    message(resp)
    return(invisible(NULL))
  }
  # Third check for bad url and stop if status > 400
  if (httr::http_error(resp)) {
    message_for_status(resp)
    return(invisible(NULL))
  }
  InputFile=read.csv(dbase_file)
  return(InputFile)
}
