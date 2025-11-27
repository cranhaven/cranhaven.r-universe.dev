#' Contruct API method requests
#'
#' @param method a character string of the method to use e.g. 'self/groups'
#' refer to https://secure.meetup.com/meetup_api.
#' @param fields a character string of the fields to be returned.
#' @param key a meetup API key.
#' @param ... additional parameters to specify for the request, must be a named
#' list where the names refer to the parameter and the arguements are characters.
#'
#' @return URL request string.
#'
#' @examples
#' .construct_req(method = "self/groups",
#'                fields = c("urlname", "id", "name"))
#'
.construct_req <- function(method, fields = "", key, ...) {

  # base URL
  url <- "https://api.meetup.com/"

  # put params in a list
  params <- list("only" = paste(fields, collapse = ","),
                 "key" = key)

  # remove only param if empty
  if (params[["only"]] == "") {
    params[["only"]] <- NULL
  }

  # if dots is specified, append
  if (!missing(...)) {
    params <- append(params, ...)
  }

  # collapse the params to format
  params <- purrr::imap(params, ~paste0("&", .y, "=", paste(.x, collapse = ",")))
  params <- paste(params, collapse = "")

  # build the request URL
  paste0(url, method, "?", params)

}

#' Clean request results
#'
#' @param content A successful request object to be parsed.
#'
#' @return returns a data.frame object of for the request. Nested fields will be
#' made into a column where the parent name is a prefix.
#' @import dplyr
#' @import purrr
#'
.clean_content <- function(content) {

  content <- content %>%
    purrr::map(~ data.frame(.x, stringsAsFactors = F)) %>%
    dplyr::bind_rows()
  names(content) <- gsub("\\.", "_", names(content))

}

#' Wrapper for processing and cleaning an API GET request
#'
#' @param method the API method string which determines the results of GET request.
#' @param fields a character string of the fields that are to be retrieved. Default
#' is to retrieve all fields.
#' @param key the API key.
#' @param dots a named list of the additional parameters to be specified for the
#' method.
#' @param only_first Boolean, default is FALSE which retrieves all records. True
#' will only get the first request.
#'
#' @return a data.frame of the results.
#' @import httr
#' @importFrom magrittr "%>%"
#'
.meetup_api_GET <- function(method, fields, key, dots, only_first = F) {

  req_url <- .construct_req(method, rev(fields), key, dots)

  resp <- httr::GET(req_url)
  httr::stop_for_status(resp)

  data <- list()
  data[[1]] <- httr::content(resp)

  # if there is no page and offset paramter, get all records
  check <- c("page", "offset") %in% names(dots)

  if(!only_first) {
    if(!any(check)) {
      total <- as.numeric(resp$headers$`x-total-count`)
      remaining <- ceiling((total - length(data[[1]]))/200)
      if (remaining > 0) {
        for (i in seq(remaining)) {
          dots[["offset"]] <- i + 1
          req_url <- .construct_req(method, rev(fields), key, dots)
          resp <- httr::GET(req_url)
          httr::stop_for_status(resp)
          data[[i + 1]] <- httr::content(resp)
        }
      }
    }
  }

  purrr::map(data, .clean_content) %>% dplyr::bind_rows()

}
