#' Get Planning Area Names
#'
#' @description
#' This function is a wrapper for the \href{https://www.onemap.gov.sg/docs/#names-of-planning-area}{Names of Planning Area API}. It returns the data as a tibble.
#'
#' @param token User's API token. This can be retrieved using \code{\link{get_token}}
#' @param year Optional, check \href{https://www.onemap.gov.sg/docs/#names-of-planning-area}{documentation} for valid options. Invalid requests will are ignored by the API.
#'
#' @return A tibble with 2 columns:
#' \describe{
#'   \item{id}{Planning area id}
#'   \item{pln_area_n}{Planning area name}
#' }
#' @export
#'
#' @examples
#' # returns tibble
#' \dontrun{get_planning_names(token)}
#' \dontrun{get_planning_names(token, 2008)}
#'
#' # error: output is NULL, warning message shows status code
#' \dontrun{get_planning_names("invalid_token")}

get_planning_names <- function(token, year = NULL) {

  # query API
  url <- "https://developers.onemap.sg/privateapi/popapi/getPlanningareaNames"
  query <- paste(url, "?",
                 "token=", token,
                 sep = "")
  if (!is.null(year)) {
    query <- paste(query,
                   "&year=", year,
                   sep = "")
  }

  response <- GET(query)

  # error handling
  if (http_error(response)) {
    status <- status_code(response)
    output <- NULL
    warning(paste("The request produced a", status, "error", sep = " "))
    return(output)

  } else {
    output <- content(response)

  }

    # convert JSON to dataframe
    output <- output %>%
      reduce(bind_rows)

    return(output)
}


