read_html_table <- function(url, css) {
  rvest::read_html(url) |>
    rvest::html_element(css = css) |>
    rvest::html_table()
}

doc_arg_local <- function(data_source) {
  paste("Whether to read the data from a local file, as opposed to the", data_source, "website.",
        "Default is `TRUE`.",
        "If the local file does not exist, will fall back to reading from online.")
}


#' Retrieve data from an Internet resource
#'
#' Performs a web request, with retries in the case of HTTP errors.
#' Returns the body of the HTTP response.
#'
#' @param url The URL to GET data from.
#' @param data_source The name of the data source.
#'  This name is used to make the error message more informative.
#'
#' @return An HTTP response body, as a UTF-8 string.
#'
#' @examples
#' # used in `get_hvw_data()`:
#' get_online_data("https://dataverse.harvard.edu/api/access/datafile/6299608", "Harvard Dataverse")
#'
#' @noRd
get_online_data <- function(url, source_name) {
  error_body <- function(response) {
    paste("ERROR", response$status_code,
          "when retrieving online data from the", source_name, "website.")
  }

  response <- httr2::request(url) |>
    httr2::req_user_agent("filibustr R package (https://cran.r-project.org/package=filibustr)") |>
    httr2::req_retry() |>
    httr2::req_error(body = error_body) |>
    httr2::req_perform()

  # return response body as UTF-8 string
  httr2::resp_body_string(response)
}
