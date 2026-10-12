
#' react to 'LIFX' api response error codes
#' @param response the api response received from httr::PUT / POST / GET
#' @return depending on the status either: an error; a warning and the response as is; the response as is without any message.
#' @references error messages copied from \url{https://api.developer.lifx.com/docs/errors}
check_lifx_response <- function(response) {
  status <- response$status_code

  # select 'LIFX' status code description (\url{https://api.developer.lifx.com/docs/errors})
  code_meaning <- switch(as.character(status), `200` = c("OK", "Everything worked as expected", "success"), `202` = c("Accepted",
                                                                                                                      "For endpoints supporting Fast Mode the request was accepted.", "success"), `207` = c("Multi-Status", "Inspect the response body to check status on individual operations.",
                                                                                                                                                                                                            "success"), `400` = c("Bad Request", "Request was invalid.", "failure"), `401` = c("Unauthorized", "Bad access token.", "failure"),
                         `403` = c("Permission Denied", "Bad OAuth scope.", "failure"), `404` = c("Not Found", "Selector did not match any lights.",
                                                                                                  "failure"), `422` = c("Unprocessable Entity", "Missing or malformed parameters.", "failure"), `426` = c("Upgrade Required",
                                                                                                                                                                                                          "HTTP was used to make the request instead of HTTPS. Repeat the request using HTTPS instead. See the Authentication section.",
                                                                                                                                                                                                          "failure"), `429` = c("Too Many Requests", "failure"), `500` = c("Server Error", "Something went wrong on \"LIFX\"'s end.",
                                                                                                                                                                                                                                                                           "failure"), `502` = c("Server Error", "Something went wrong on \"LIFX\"'s end.", "failure"), `503` = c("Server Error", "Something went wrong on LIFX's end.",
                                                                                                                                                                                                                                                                                                                                                                                  "failure"), `523` = c("Server Error", "Something went wrong on \"LIFX\"'s end.", "failure"))

  # return object, throw warning or stop depending on status code:
  if (is.null(code_meaning)) {
    stop(
      paste0(paste(status, "unknown API status code (not documented in https://api.developer.lifx.com/docs/errors")),
      '\n response: \n',
      as.character(response))
  }

  if (code_meaning[3] == "success") {
    return(invisible(response))
  }

  if (code_meaning[3] == "warning") {
    warning(paste0(status, " - ", code_meaning[1], ": ", code_meaning[2]))
    return(response)
  }

  if (code_meaning[3] == "failure") {
    stop(paste0(paste0(status, " - ", code_meaning[1], ": ", code_meaning[2]), "\n", httr::content(response)$error))
  }

  return(invisible(response))

}
