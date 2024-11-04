#' PSI(PoullutantStandardIndex)
#'
#' This functions calls upon the PSI API from data.gov.sg
#' and returns a data frame of the different measures of the PSI across 5
#' different areas in Singapores and the overall measure for the given
#' data-time. This data provided by the API is updated hourly.
#'
#' Note that this function is different from the `PSI_summary` function,
#' which returns the PSI measures for a given day.
#'
#' @param date_time Defaults to current (SGD) time. Format: YYYY-MM-DDTHH:MM:SS
#'
#' @keywords PSI
#'
#' @return A dataframe containing various PSI measures across 5 corners
#' of Singapore
#'
#' @export
#' @examples
#' psi()
#' psi(date = "2019-11-08T17:30:00")
#' psi(date = "2018-01-04T09:16:17")

psi = function(date_time = "") {

  # Creating and pulling URL
  URL = parse_api_date(api = "environment/psi",
                       input_date = date_time,
                       summary = FALSE)

  output = tryCatch({httr::GET(URL)},
                    error = function(e) {
                      if (inherits(e, "TimeoutError")) {return(NULL)}
                      else {stop(e)}
                      }
                    )

  if (is.null(output)) {
    message("API request timed out.")
    return(NULL)
  }

  # Content check
  content.output = parse_api_output(output)

  if (length(content.output$items) == 0) {
    message("No data returned from API.")
    return(NULL)
  }

  # Extracting Data Frame
  message("Closest timestamp: ", content.output$items[[1]]$timestamp)

  psi = cbind(psi_measures = names(content.output$items[[1]]$readings),
              dplyr::bind_rows(content.output$items[[1]]$readings))

  return(psi)

}
