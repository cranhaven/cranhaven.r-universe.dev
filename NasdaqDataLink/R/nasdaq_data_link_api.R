#' Executes Nasdaq Data Link API calls
#'
#' @details Set your \code{api_key} with \code{NasdaqDataLink.api_key} function. For instructions on finding your api key go to \url{https://data.nasdaq.com/account/profile}
#'
#' @param path Path to api resource.
#' @param http Type of http request sent.
#' @param postdata A character or raw vector that is sent in a body.
#' @param ... Named values that are interpretted as Nasdaq Data Link API parameters. Please see \url{https://docs.data.nasdaq.com/}.
#' @return Nasdaq Data Link API response.
#' @seealso \code{\link{NasdaqDataLink.api_key}}
#' @examples \dontrun{
#' data_link_data = nasdaq_data_link.api(path="datasets/NSE/OIL", http="GET")
#' plot(data_link_data[,1])
#' }
#' @importFrom httr VERB
#' @importFrom jsonlite fromJSON
#' @importFrom methods is
#' @export
nasdaq_data_link.api <- function(path, http = c("GET", "PUT", "POST", "DELETE"), postdata = NULL, ...) {
  http <- match.arg(http)
  request <- nasdaq_data_link.api.build_request(path, ...)
  response <- httr::VERB(http, request$request_url,
                         config = do.call(httr::add_headers, request$headers),
                         body = postdata, query = request$params)

  nasdaq_data_link.api.handl_errors(response)
  text_response <- httr::content(response, as = "text")

  json_response <- tryCatch(jsonlite::fromJSON(text_response, simplifyVector = TRUE), error = function(e) {
      stop(e, " Failed to parse response: ", text_response)
    })
  json_response
}

nasdaq_data_link.api.download_file <- function(path, filename, ...) {
  request <- nasdaq_data_link.api.build_request(path, ...)
  response <- httr::GET(request$request_url,
                        config = do.call(httr::add_headers, request$headers),
                        query = request$params,
                        httr::write_disk(filename, overwrite = TRUE),
                        httr::progress())
  nasdaq_data_link.api.handl_errors(response)
  cat("Saved to file:", response$content)
}

nasdaq_data_link.api.build_request <- function(path, ...) {
  params <- list(...)
  # ensure vectors get converted into v3 api supported query params
  # e.g., qopts.columns=c('ticker', 'rev') -> list('qopts.columns[]'=ticker,'qopts.columns[]'=rev)
  params <- nasdaq_data_link.api.build_query_params(params)
  # ensure Dates convert to characters or else curl will convert the Dates to timestamp
  params <- nasdaq_data_link.api.convert_dates_to_character(params)

  request_url <- paste(NasdaqDataLink.base_url(), path, sep = "/")
  accept_value <- "application/json"
  if (!is.null(NasdaqDataLink.api_version())) {
    accept_value <- paste0("application/json, application/vnd.nasdaq_data_link+json;version=", NasdaqDataLink.api_version())
  }

  nasdaq_data_link_version <- as.character(utils::packageVersion("NasdaqDataLink"))
  headers <- list(Accept = accept_value, `Request-Source` = "R", `Request-Source-Version` = nasdaq_data_link_version)

  if (!is.null(NasdaqDataLink.api_key())) {
    headers <- c(headers, list(`X-Api-Token` = NasdaqDataLink.api_key()))
  }

  # query param api_key takes precedence
  if (!is.null(params$api_key)) {
    headers <- c(headers, list(`X-Api-Token` = params$api_key))
    params$api_key <- NULL
  }

  list(request_url = request_url, headers = headers, params = params)
}

nasdaq_data_link.api.handl_errors <- function(response) {
  if (!(httr::status_code(response) >= 200 && httr::status_code(response) < 300)) {
    stop(httr::content(response, as = "text"), call. = FALSE)
  }
}

nasdaq_data_link.api.convert_dates_to_character <- function(params) {
  convert_date_to_character <- function(param) {
    if (is(param, "Date")) {
      param <- as.character(param)
    }
    param
  }
  lapply(params, convert_date_to_character)
}

nasdaq_data_link.api.build_query_params <- function(params) {
  if (length(params) <= 0) {
    return(params)
  }
  mod_params <- list()
  for(i in 1:length(params)) {
    # keep the params the same if not a vector
    converted_params <- params[i]

    # check val to see if vector
    # if so, convert
    if (length(params[[i]]) > 1) {
      converted_params <- nasdaq_data_link.api.convert_vector_params(names(params[i]), params[[i]])
    }
    mod_params <- c(mod_params, converted_params)
  }
  return(mod_params)
}

nasdaq_data_link.api.convert_vector_params <- function(name, vector_values) {
  mod_query_name <- paste0(name, "[]")
  mod_query_list <- list()

  for(val in vector_values) {
    l <- list()
    l[[mod_query_name]] <- val
    mod_query_list <- c(mod_query_list, l)
  }
  return(mod_query_list)
}
