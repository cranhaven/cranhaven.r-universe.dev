#' Perform an API request to BCRPData
#' @param codes A character vector with valid BCRPData series codes, see the [get_bcrp_metadata()] function. Required.
#' @param from A character vector of length 1, usually a year see Details. Optional.
#' @param to A character vector of length 1. Must be greater than the
#' `from` argument. Optional.
#' @param request_strategy Either "sequential" (default) or "parallel". This defines the strategy to be followed when making requests for more than one code. Visit [httr2](https://httr2.r-lib.org/reference/index.html#perform-multiple-requests) for more details.
#' @details
#' It is possible to specify only the `from` or `to` arguments in which case the request to the BCRPData API would only take into consideration the non-missing argument, if both are ommited the API will respond with the latest data.
#'
#' BCRPData has data of different frequencies, it's important to consider that when defining the `from` and `to` arguments. Here's a list you can use to define the most suitable values to the `from` and `to` arguments.
#' \itemize{
#'
#' \item{Yearly: Provide a year, e.g 2018}
#' \item{Quarterly: Provide a year followed by a hyphen followed by the quarter in its numerical value, e.g 2018-2}
#' \item{Monthly: Provide a year followed by a hyphen followed by the month in its numerical value, e.g 2018-5}
#' \item{Daily: Provide a year followed by a hyphen followed by the month and followed by the day, e.g 2018-5-5}
#' }
#' This function will try to get all the valid codes you provided in its `codes` argument, if one of the requests fails it will stop its execution for all the codes and return an error, pointing out which codes caused this.
#'
#' @export
#' @examples
#' codes <- c("PN00009MM", "PN00002MM", "PN01270PM", "PD39557DA")
#' # Will get you the most recent data for these codes
#' # as you have not provided the `from` and `to` arguments.
#' get_bcrp_data(codes = codes)
#'
#' # You can also provide the range of dates
#' # through the `from` and `to` arguments.
#' get_bcrp_data(codes = codes, from = "2015-01", to = "2020-01")
#'
#' # If needed, you can sometimes speed up your data API calls
#' # by performing them in parallel. In this example we are
#' # querying just 5 codes so the benefits of parallelism does not
#' # outweigh its cost, making the request slower.
#' \dontrun{
#' get_bcrp_data(codes = codes, request_strategy = "parallel")
#' }
#' @return Data frame with code-level observations.
#' @author Julio Collazos.
get_bcrp_data <- function(
  codes,
  from = NA,
  to = NA,
  request_strategy = c("sequential", "parallel")
) {
  request_strategy <- match.arg(arg = request_strategy)
  range <- c(from, to)
  if (all(!is.na(range)) && substring(from, 1, 4) > substring(to, 1, 4)) {
    stop("The value of 'from' must be below the value of 'to'")
  }
  base_uri <- "https://estadisticas.bcrp.gob.pe/estadisticas/series/api/"
  range <- paste0("/", range[!is.na(range)], collapse = "")
  list_of_requests <- lapply(
    paste0(base_uri, codes, "/json", range),
    httr2::request
  )

  list_of_responses <-
    tryCatch(
      perform_req_strategy(
        requests = list_of_requests,
        strategy = request_strategy
      ),
      error = function(e) {
        conditionMessage(e)
      }
    )

  if (!is.list(list_of_responses)) {
    message(list_of_responses)
    return(invisible(NULL))
  }

  tbl <- lapply(list_of_responses, function(s) {
    f_response <- httr2::resp_body_raw(s)
    f_response <- yyjsonr::read_json_raw(f_response)
    code <- get_underlying_code(request_string = s$url)
    series_name <- f_response$config$series$name
    df <- f_response$periods
    df$values <- unlist(df$values)
    df$series_name <- series_name
    df$code <- code
    df
  })
  tibble::tibble(do.call(rbind, tbl))
}
