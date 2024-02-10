#' Extract API token from OneMap.Sg
#'
#' @description
#' This function is a wrapper for the \href{https://www.onemap.gov.sg/docs/#authentication-service-post}{OneMap Authentication Service API}. It allows users to generate a API token from OneMap.Sg.
#' Using the API requires that users have a registered email address with Onemap.Sg. Users can register themselves using \href{https://developers.onemap.sg/register/}{OneMap.Sg's form}.
#'
#' @param email User's registered email address.
#' @param password User's password.
#' @param hide_message Default = \code{FALSE}. Whether to hide message telling user when the token expires.
#' @return API token, or NULL if an error occurs. If error occurs, a warning message will be printed with the error code.
#' @export
#'
#' @examples
#' \dontrun{get_token("user@@example.com",  "password")}

get_token <- function(email, password, hide_message = FALSE) {
  # query API
  url <- "https://developers.onemap.sg/privateapi/auth/post/getToken"
  details <- list(email = email, password = password)
  response <- POST(url, body = details, encode = "json")

  # error handling
  if (http_error(response)) {
    status <- status_code(response)
    output <- NULL
    warning(paste("The request produced a", status, "error", sep = " "))
    return(output)

  # else return results
  } else {
    content <- content(response)

    if (names(content)[[1]] == "error") {
      output <- NULL
      warning(content$error)
      return(output)
    }

    output <- content$access_token

    # optionally, tell user when the token will expire
    if (!hide_message) {
      expiry_time <- as.POSIXct(as.integer(content$expiry_timestamp),
                                origin = "1970-01-01", tz = Sys.timezone())
      message(paste("This token will expire on", expiry_time))
    }

  }

  output

}
