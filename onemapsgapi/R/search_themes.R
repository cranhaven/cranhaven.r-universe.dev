#' Search for Themes available on OneMap.Sg
#'
#' @description
#' This function is a wrapper for the \href{https://www.onemap.gov.sg/docs/#get-all-themes-info}{Get All Themes Info API}. It allows users to get a tibble of all available themes, and their details, in the OneMap.Sg API. It also provides an additional functionality where users can subset their results using search terms.
#'
#' @param token User's API token. This can be retrieved using \code{\link{get_token}}
#' @param ... Optional Search terms to subset results; results with any of search terms will be returned. Search terms are not case-sensitive.
#' @param more_info Whether more infomation should be queried, default = \code{TRUE}. If \code{FALSE}, output will contain Theme Name, Query Name and Icon information. If \code{TRUE}, output will additionally contain Category and Theme Owner information.
#' @return If no error occurs, a tibble with the following variables:
#' \describe{
#'   \item{THEMENAME}{Name of the Theme}
#'   \item{QUERYNAME}{Query name of the Theme}
#'   \item{ICON}{Name of image file used as Icon in OneMap Web Map}
#'   \item{CATEGORY}{Returned only if \code{more_info = TRUE}. Topic that Theme relates to, e.g. Health, Sports, Environment, etc.}
#'   \item{THEME_OWNER}{Returned only if \code{more_info = TRUE}. Government Agency who Owns the Dataset}
#' }
#'
#' If an error occurs, the function returns \code{NULL}, along with a warning message.
#'
#' @export
#' @examples
#' # valid
#' \dontrun{search_themes(token)}
#' \dontrun{search_themes(token, "hdb", "parks")}
#' \dontrun{search_themes(token, more_info = FALSE)}
#'
#' # error
#' \dontrun{search_themes("my_invalid_token")}

search_themes <- function(token, ..., more_info = TRUE) {
  # query API
  url <- "https://developers.onemap.sg/privateapi/themesvc/getAllThemesInfo?"
  more_info <- if_else(more_info, "Y", "N")
  query <- paste(url,
                 "token=", token,
                 "&moreInfo=", more_info,
                 sep = "")
  response <- GET(query)

  # error handling
  if (http_error(response)) {
    status <- status_code(response)
    output <- NULL
    warning(paste("The request produced a", status, "error", sep = " "))

  # else return output
  } else {
    output <- content(response) %>%
      flatten() %>%
      bind_rows()

    # subset output if user is searching for a term
    search <- c(...)

    if (length(search) != 0) {
      search <- str_c(search, collapse = "|")

      search_matches <- output %>%
        unite(col = "combined", sep = " ") %>%
        unlist() %>%
        map_lgl(str_detect, pattern = regex(search, ignore_case = TRUE))

      output <- output[search_matches, ]
    }

  }

  output

}
