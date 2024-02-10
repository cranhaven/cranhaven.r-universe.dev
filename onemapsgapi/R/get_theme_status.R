#' Check Theme Status
#'
#' @description
#' This function is a wrapper for the \href{https://www.onemap.gov.sg/docs/#check-theme-status}{Check Theme Status API}. It returns a named logical indicating if the theme is updated at a specific date.
#'
#' @param token User's API token. This can be retrieved using \code{\link{get_token}}
#' @param theme Query name of theme. Themes’ query names can be retrieved using \code{\link{search_themes}}.
#' @param date Default = current date. Date to check for updates. Format YYYY-MM-DD
#' @param time Default = current time. Time to check for updates. Format: HH:MM:SS:FFFZ
#'
#' @return A named logical indicating if the theme is updated at a specific date.
#' If an error occurred, the function returns \code{NULL} along with a warning message.
#'
#' @export
#'
#' @examples
#' # returns named logical
#' \dontrun{get_theme_status(token, "kindergartens")}
#' \dontrun{get_theme_status(token, "hotels", "2020-01-01", "12:00:00")}
#'
#' # returns NULL, warning message shows status code
#' \dontrun{get_theme_status("invalid_token", "blood_bank")}
#'
#' # returns NULL, warning message shows error
#' \dontrun{get_theme_status(token, "invalid_theme")}

get_theme_status <- function(token, theme, date = Sys.Date(), time = format(Sys.time(), format = "%T")) {

  date_time <- paste0(date, "T", time)

  # query API
  url <- "https://developers.onemap.sg/privateapi/themesvc/checkThemeStatus"

  query <- paste(url, "?",
                 "token=", token,
                 "&queryName=", theme,
                 "&dateTime=", date_time,
                 sep = "")

  response <- GET(query)

  # error handling
  if (http_error(response)) {
    status <- status_code(response)
    output <- NULL
    warning(paste("The request produced a", status, "error", sep = " "))
    return(output)

  }

  output <- content(response)

  if (names(output)[1] == "error") {
    warning(output$error)
    output <- NULL
    return(output)

  } else {
    output <- output %>%
      unlist()

  }

  return(output)

}
