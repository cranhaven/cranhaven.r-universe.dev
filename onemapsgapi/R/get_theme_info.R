#' Get Theme Information
#'
#' @description
#' This function is a wrapper for the \href{https://www.onemap.gov.sg/docs/#get-theme-info}{Get Theme Info API}. It returns a named character vector of Theme Name and Query Name.
#'
#' @param token User's API token. This can be retrieved using \code{\link{get_token}}
#' @param theme Query name of theme. Themes’ query names can be retrieved using \code{\link{search_themes}}.
#'
#' @return A named character vector of Theme Name and Query Name.
#' If an error occurred, the function returns \code{NULL} along with a warning message.
#'
#' @export
#'
#' @examples
#' # returns named character vector
#' \dontrun{get_theme_status(token, "kindergartens")}
#'
#' # returns NULL, warning message shows status code
#' \dontrun{get_theme_status("invalid_token", "blood_bank")}
#'
#' # returns NULL, warning message shows error
#' \dontrun{get_theme_status(token, "invalid_theme")}

get_theme_info <- function(token, theme) {

  # query API
  url <- "https://developers.onemap.sg/privateapi/themesvc/getThemeInfo"

  query <- paste(url, "?",
                 "token=", token,
                 "&queryName=", theme,
                 sep = "")

  response <- GET(query)

  # error handling
  if (http_error(response)) {
    status <- status_code(response)
    output <- NULL
    warning(paste("The request produced a", status, "error", sep = " "))
    return(output)

  }

  if (names(response)[1] == "error") {
    warning(response$error)
    output <- NULL
    return(output)

  } else {
    output <- content(response) %>%
      unlist()

  }

  return(output)
}
