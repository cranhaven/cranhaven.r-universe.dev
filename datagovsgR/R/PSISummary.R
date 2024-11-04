#' PSI(PoullutantStandardIndex) Summary
#'
#' This functions calls upon the PSI API from data.gov.sg
#' and returns a data frame of the different measures of the PSI across 5
#' different areas in Singapores and the overall measure for the given
#' date. This data provided by the API is updated hourly.
#'
#' Note that this function is different from the `PSI` function,
#' which returns the PSI measures for a given date-time.
#'
#' @param date Defaults to current (SGD) time. Format: YYYY-MM-DDTHH:MM:SS
#'
#' @keywords PSI
#'
#' @return A dataframe containing various PSI measures across 5 corners
#' of Singapore and time of day. Dependent on the data availible, not all
#' results range from 0000 to 2300.
#'
#' @importFrom dplyr %>%
#'
#' @export
#' @examples
#' psi_summary()
#' psi_summary(date = "2019-11-08")
#' psi_summary(date = "2018-01-04")

psi_summary = function(date = "") {

  # Creating and pulling URL
  URL = parse_api_date(api = "environment/psi",
                       input_date = date,
                       summary = TRUE)
  output = httr::GET(URL)

  # Error check
  content.output = parse_api_output(output)

  if (length(content.output$items) == 0) {
    stop("No data returned from API.")
  }

  # Extracting Data Frame

  psi_summary = lapply(1:length(content.output$items), function(x) {

    psi_summary = content.output$items[[x]]$readings %>%
      dplyr::bind_rows() %>%
      data.table::transpose()
    colnames(psi_summary) = names(content.output$items[[x]]$readings)
    psi_summary = cbind(date_time = content.output$items[[x]]$timestamp,
                        region = names(content.output$items[[x]]$readings[[1]]),
                        psi_summary,
                        stringsAsFactors = FALSE)

  })

  psi_summary = dplyr::bind_rows(psi_summary)

  return(psi_summary)

}

