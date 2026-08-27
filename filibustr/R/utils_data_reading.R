#' Retrieve data from an Internet resource
#'
#' Performs a web request, retrying up to 3 times in the case of HTTP errors.
#' Returns the body of the HTTP response.
#'
#' @param url The URL to GET data from.
#' @param data_source The name of the data source.
#'  This name is used to make the error message more informative.
#' @param return_format The desired format for the response body.
#'  Supported options include `"string"` and `"raw"`, which correspond to
#'  [httr2::resp_body_string()] (UTF-8 string) and [httr2::resp_body_raw()]
#'  (raw bytes), respectively. Default is `"string"`.
#'
#' @return An HTTP response body, as a UTF-8 string.
#'
#' @examplesIf !is.null(curl::nslookup("dataverse.harvard.edu", error = FALSE))
#' # used in `get_hvw_data()`:
#' get_online_data("https://dataverse.harvard.edu/api/access/datafile/6299608", "Harvard Dataverse")
#'
#' @noRd
get_online_data <- function(url, source_name, return_format = "string") {
  error_body <- function(response) {
    paste("ERROR", response$status_code,
          "when retrieving online data from the", source_name, "website.")
  }

  response <- httr2::request(url) |>
    httr2::req_user_agent("filibustr R package (https://cran.r-project.org/package=filibustr)") |>
    httr2::req_retry(max_tries = 3) |>
    httr2::req_error(body = error_body) |>
    httr2::req_perform()

  # return response body
  if (return_format == "raw") {
    # raw bytes
    return(httr2::resp_body_raw(response))
  } else {
    # default: UTF-8 string
    return(httr2::resp_body_string(response))
  }
}

read_local_file <- function(path, ...) {
  file_ending <- tools::file_ext(x = path)
  switch(file_ending,
         csv = readr::read_csv(file = path, ...),
         tsv = readr::read_tsv(file = path, ...),
         tab = readr::read_tsv(file = path, ...),
         dta = haven::read_dta(file = path),
         cli::cli_abort(
           c(
             "Invalid {.arg path} provided:",
             "x" = "{.arg {path}}",
             "i" = "File must be in one of the following formats: .csv, .dta, .tab, .tsv"
           ),
           call = rlang::caller_env()
         ))
}

# get Voteview data for multiple Congresses, one-by-one
# using {purrr}'s parallelism through {mirai}
# if {mirai} and {carrier} are installed
# and mirai daemons are set up
multi_congress_read <- function(fun, chamber, congress) {
  .f <- if (rlang::is_installed(c("carrier", "mirai"), version = c("0.3.0",  "2.5.1")) &&
            !is.null(mirai::info())) {
    purrr::in_parallel(
      function(.cong) fun(chamber = chamber, congress = .cong),
      fun = fun,
      chamber = chamber
    )
  } else {
    function(.cong) fun(chamber = chamber, congress = .cong)
  }

  purrr::map(congress, .f = .f) |>
    purrr::list_rbind()
}
