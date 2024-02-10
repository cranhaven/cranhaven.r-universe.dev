#' Get Population Data (Multiple)
#'
#' @description
#' This function is a wrapper for the \href{https://www.onemap.gov.sg/docs/#population-query}{Population Query API}. It allows for querying of multiple Popquery data types for multiple towns and years.
#'
#' @param token User's API token. This can be retrieved using \code{\link{get_token}}
#' @param data_types Type of data to be retrieved, should correspond to one of the API endpoints. E.g. to get economic status data, \code{data_type = "getEconomicStatus"}. The API endpoints can be found on the documentation page.
#' @param planning_areas Town for which the data should be retrieved.
#' @param years Year for which the data should be retrieved.
#' @param gender Optional, if specified only records for that gender will be returned. This parameter is only valid for the \code{"getEconomicStatus"}, \code{"getEthnicGroup"}, \code{"getMaritalStatus"} and \code{"getPopulationAgeGroup"} endpoints. If specified for other endpoints, the parameter will be dropped.
#' @param parallel Default = \code{FALSE}. Whether to run API calls in parallel or sequentially (default). Enabling parallel iterations is highly recommended for when querying multiple data types/years/towns.
#' @return A tibble with each row representing a town in a particular year for a particular gender, and columns with the variables returned by the API endpoint.
#' If any API call returns no data, the values will be NA but the row will be returned. However, if all data_types do not return data for that town and year, no row will be returned for it.
#'
#' @export
#'
#' @examples
#' # output with no NA
#' \dontrun{get_pop_queries(token, c("getOccupation", "getLanguageLiterate"),
#'     c("Bedok", "Yishun"), "2010")}
#' \dontrun{get_pop_queries(token, c("getEconomicStatus", "getEthnicGroup"),
#'     "Yishun", "2010", "female")}
#'
#' ## note behaviour if data types is a mix of those that accept gender params
#' ### only total will have all records
#' \dontrun{get_pop_queries(token, c("getEconomicStatus", "getOccupation", "getLanguageLiterate"),
#'     "Bedok", "2010")}
#' ### data type that does not accept gender params will be in gender = Total
#' \dontrun{get_pop_queries(token, c("getEconomicStatus", "getOccupation", "getLanguageLiterate"),
#'     "Bedok", "2010", gender = "female")}
#'
#' # output with some town-year queries without record due to no data
#' # warning message will show data_type/town/year/gender for which an error occurred
#' \dontrun{get_pop_queries(token, c("getEconomicStatus", "getOccupation"),
#'     "Bedok", c("2010", "2012"))} # no records for 2012


get_pop_queries <- function(token, data_types, planning_areas, years, gender = NULL, parallel = FALSE) {

  # make tibble of query params
  query_params <- crossing(planning_areas, years)

  # preallocate output list
  output_list <- as.list(rep(NA, length(data_types)))
  names(output_list) <- as.character(data_types)

  if (parallel) {
    if (Sys.info()[["sysname"]] == "Windows") {plan(multisession)} else {plan(multicore)}
  }

  # query params for each data type
  for (i in data_types) {

    # iterate parallel or sequentially through query params
    if (parallel) {
      query_outputs <- future_pmap(query_params, function(planning_areas, years) get_pop_query(token = token, data_type = i, planning_area = planning_areas, year = years, gender = gender))
      plan(sequential)

    } else {

      query_outputs <- pmap(query_params, function(planning_areas, years) get_pop_query(token = token, data_type = i, planning_area = planning_areas, year = years, gender = gender))
    }

     query_outputs <- reduce(query_outputs, bind_rows)

    # add each tibble, containing all query params for one data_type into a list
    output_list[[i]] <- query_outputs
  }

  # join all tibble in list into single tibble
  if (sum(map_int(output_list, length)) == 0) {
    output <- NULL
    warning("Your request produced 0 outputs! Please check your query and refer to documentation for valid parameters.")

  } else {
    output <- output_list %>%
      reduce(full_join, by = c("planning_area", "year", "gender"))
  }

  return(output)

}
