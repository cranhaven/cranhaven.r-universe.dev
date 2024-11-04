#' ParseApiOutput
#'
#' Helper function to extract the content returned from the API.
#' Returns the status code otherwise.
#'
#' @param inputcontent Takes the output of the GET function, runs an error check and returns the parsed output.
#'
#' @return The extracted content if not error has occured. Otherwise, the error message is returned.

parse_api_output = function(inputcontent) {

  if (!httr::http_error(inputcontent)) {

    return(httr::content(inputcontent))

  } else if (httr::status_code(inputcontent) >= 400 &
             httr::status_code(inputcontent) < 500) {

    stop("Client error has occured. Check your code.")

  } else if (httr::status_code(inputcontent) >= 500 &
             httr::status_code(inputcontent) < 600) {

    stop("Server Error has occured.")

  }
}
