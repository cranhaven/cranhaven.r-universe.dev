#' Perform the requests list
#'
#' @noRd
#' @keywords internal
perform_req_strategy <- function(requests, strategy) {
  f_strategy <- switch(
    strategy,
    "sequential" = httr2::req_perform_sequential,
    "parallel" = httr2::req_perform_parallel
  )
  res <- f_strategy(requests, on_error = "continue")
  failures <- httr2::resps_failures(res)

  if (length(failures) > 0) {
    errs <- seq_along(res)[sapply(res, function(s) {
      inherits(s, "httr2_error")
    })]
    urls <- sapply(requests[errs], function(s) s$url)
    errs <- paste(get_underlying_code(urls), collapse = " ")
    stop(
      "Error(s) at: ",
      errs,
      "\n",
      "Hint: Make sure you got the correct codes."
    )
  } else {
    res
  }
}


#' Get Underlying API code
#'
#' @noRd
#' @keywords internal
get_underlying_code <- function(request_string) {
  regmatches(
    x = request_string,
    m = regexpr(
      "(?<=api\\/)[A-Z0-9]{1,}(?=\\/json)",
      request_string,
      perl = TRUE
    )
  )
}
